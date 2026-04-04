;;; discourse-ui.el --- Buffer-based UI for Discourse  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2024 The Authors of discourse.el

;; This file is part of discourse.el.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Buffer-based UI for browsing Discourse forums.
;; Provides three main views:
;;   - Category list (tabulated-list-mode)
;;   - Topic list (tabulated-list-mode)
;;   - Topic/post thread viewer (special-mode)

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'shr)
(require 'mm-url)
(require 'discourse-api)

(declare-function discourse-compose--reply "discourse-compose" (topic-id &optional reply-to-post-number topic-title reply-to-username))
(declare-function discourse-compose--new-topic "discourse-compose" (category-id &optional category-name))
(declare-function discourse-mark-topic-read "discourse" (topic-id highest-post-number))
(declare-function discourse-topic-read-post-number "discourse" (site-url topic-id))

;;; --- Customization ---

(defgroup discourse-ui nil
  "Discourse UI settings."
  :group 'discourse
  :prefix "discourse-ui-")

(defcustom discourse-ui-topic-page-size 30
  "Number of topics to display per page."
  :type 'integer
  :group 'discourse-ui)

(defcustom discourse-ui-render-html t
  "If non-nil, render post HTML via `shr'.
Otherwise display raw HTML."
  :type 'boolean
  :group 'discourse-ui)

(defcustom discourse-ui-sidebar-width 35
  "Width of the sidebar window in columns."
  :type 'integer
  :group 'discourse-ui)

(defvar discourse-ui--sidebar-buffer nil
  "The sidebar buffer for the current Discourse session.")

(defface discourse-ui-category-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for category names."
  :group 'discourse-ui)

(defface discourse-ui-topic-title-face
  '((t :inherit font-lock-function-name-face))
  "Face for topic titles."
  :group 'discourse-ui)

(defface discourse-ui-pinned-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for pinned topics."
  :group 'discourse-ui)

(defface discourse-ui-author-face
  '((t :inherit font-lock-variable-name-face))
  "Face for post author names."
  :group 'discourse-ui)

(defface discourse-ui-date-face
  '((t :inherit font-lock-comment-face))
  "Face for dates."
  :group 'discourse-ui)

(defface discourse-ui-unread-face
  '((((class color) (background dark))
     :weight bold :foreground "#51afef")
    (((class color) (background light))
     :weight bold :foreground "#0054b6")
    (t :weight bold))
  "Face for unread/unseen topics."
  :group 'discourse-ui)

(defface discourse-ui-read-face
  '((t :inherit shadow))
  "Face for already-read topics."
  :group 'discourse-ui)

(defface discourse-ui-post-separator-face
  '((t :inherit font-lock-comment-face))
  "Face for the separator between posts."
  :group 'discourse-ui)

(defface discourse-ui-likes-face
  '((t :inherit font-lock-constant-face))
  "Face for like counts."
  :group 'discourse-ui)

;;; --- Time formatting (ported from nndiscourse) ---

(defun discourse-ui--dense-time (time)
  "Convert TIME to a floating point number.
Written by John Wiegley (https://github.com/jwiegley/dot-emacs)."
  (float-time time))

(defun discourse-ui--format-time-elapsed (date-string)
  "Return human-readable time elapsed since DATE-STRING."
  (condition-case nil
      (when (and date-string (> (length date-string) 0))
        (let* ((then (float-time (date-to-time date-string)))
               (now (float-time (current-time)))
               (diff (- now then)))
          (cond
           ((>= diff (* 86400 365))
            (format "%dy" (floor (/ diff (* 86400 365)))))
           ((>= diff (* 86400 30))
            (format "%dmo" (floor (/ diff (* 86400 30)))))
           ((>= diff (* 86400 7))
            (format "%dw" (floor (/ diff (* 86400 7)))))
           ((>= diff 86400)
            (format "%dd" (floor (/ diff 86400))))
           ((>= diff 3600)
            (format "%dh" (floor (/ diff 3600))))
           ((>= diff 60)
            (format "%dm" (floor (/ diff 60))))
           (t (format "%ds" (floor diff))))))
    (error "?")))

;;; --- HTML rendering (ported from nndiscourse) ---

(defun discourse-ui--render-html (html)
  "Render HTML string to plain text using `shr'."
  (if (not discourse-ui-render-html)
      html
    (with-temp-buffer
      (insert html)
      (let ((shr-width (min (- (window-width) 4) 80))
            (shr-use-fonts nil))
        (shr-render-region (point-min) (point-max)))
      (buffer-string))))

(defun discourse-ui--massage-html (body)
  "Clean up Discourse cooked HTML in BODY for display.
Ported from nndiscourse."
  (when body
    (with-temp-buffer
      (insert body)
      (mm-url-decode-entities)
      ;; Strip srcset attributes that confuse shr image rendering
      (goto-char (point-min))
      (while (re-search-forward "\\s-+srcset=\"[^\"]*\"" nil t)
        (replace-match ""))
      ;; Strip data-* attributes
      (goto-char (point-min))
      (while (re-search-forward "\\s-+data-[a-z-]+=\"[^\"]*\"" nil t)
        (replace-match ""))
      ;; Strip loading="lazy" which shr doesn't understand
      (goto-char (point-min))
      (while (re-search-forward "\\s-+loading=\"[^\"]*\"" nil t)
        (replace-match ""))
      (buffer-string))))

;;; ======================================================================
;;; Category List View (Sidebar-style)
;;; ======================================================================

(defvar-local discourse-ui--categories-data nil
  "Cached category data for the current buffer.")

(defvar-local discourse-ui--nav-items nil
  "Navigation item counts: plist with :unread :new :messages.")

(defvar discourse-ui-categories-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'discourse-ui-activate-item)
    (define-key map (kbd "g") #'discourse-ui-refresh-categories)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "s") #'discourse-ui-search)
    (define-key map (kbd "o") #'discourse-ui-focus-main)
    (define-key map (kbd "n") #'discourse-ui-sidebar-next-item)
    (define-key map (kbd "p") #'discourse-ui-sidebar-prev-item)
    (define-key map (kbd "TAB") #'discourse-ui-sidebar-next-item)
    (define-key map (kbd "<backtab>") #'discourse-ui-sidebar-prev-item)
    map)
  "Keymap for `discourse-ui-categories-mode'.")

(define-derived-mode discourse-ui-categories-mode special-mode
  "Discourse"
  "Major mode for browsing Discourse forum — sidebar style."
  (setq truncate-lines t
        buffer-read-only t))

(defun discourse-ui--sidebar-window ()
  "Return the sidebar window if it is currently visible, or nil."
  (when (and discourse-ui--sidebar-buffer
             (buffer-live-p discourse-ui--sidebar-buffer))
    (get-buffer-window discourse-ui--sidebar-buffer)))

(defun discourse-ui--ensure-sidebar ()
  "Ensure the sidebar window is visible on the left.
Returns the sidebar window."
  (let ((sidebar-win (discourse-ui--sidebar-window)))
    (unless sidebar-win
      (when (and discourse-ui--sidebar-buffer
                 (buffer-live-p discourse-ui--sidebar-buffer))
        (setq sidebar-win
              (display-buffer-in-side-window
               discourse-ui--sidebar-buffer
               `((side . left)
                 (slot . 0)
                 (window-width . ,discourse-ui-sidebar-width)
                 (preserve-size . (t . nil)))))
        (when sidebar-win
          (set-window-dedicated-p sidebar-win t))))
    sidebar-win))

(defun discourse-ui-focus-sidebar ()
  "Switch focus to the sidebar window."
  (interactive)
  (let ((sidebar-win (discourse-ui--sidebar-window)))
    (if sidebar-win
        (select-window sidebar-win)
      (message "Sidebar not visible.  Press M-x discourse to open it."))))

(defun discourse-ui-focus-main ()
  "Switch focus from the sidebar to the main content window."
  (interactive)
  (let* ((sidebar-win (discourse-ui--sidebar-window))
         (main-win (when sidebar-win
                     (seq-find (lambda (w)
                                 (and (not (eq w sidebar-win))
                                      (not (window-minibuffer-p w))))
                               (window-list)))))
    (if main-win
        (select-window main-win)
      (message "No content window open.  Select a category first."))))

(defun discourse-ui--display-in-main (buffer)
  "Display BUFFER in the main (non-sidebar) window.
Ensures the sidebar is visible and shows BUFFER alongside it."
  (let ((sidebar-win (discourse-ui--ensure-sidebar)))
    (if sidebar-win
        ;; Find or create a window that isn't the sidebar
        (let ((main-win (seq-find (lambda (w)
                                    (and (not (eq w sidebar-win))
                                         (not (window-minibuffer-p w))))
                                  (window-list))))
          (if main-win
              (progn
                (set-window-buffer main-win buffer)
                (select-window main-win))
            ;; No main window found, split from root
            (select-window (split-window (frame-root-window) nil 'right))
            (switch-to-buffer buffer)))
      ;; No sidebar — just switch normally
      (switch-to-buffer buffer))))

(defun discourse-ui--hex-to-color (hex)
  "Convert a HEX color string (without #) to an Emacs color string."
  (if (and hex (> (length hex) 0))
      (concat "#" hex)
    "#888888"))

(defun discourse-ui--render-sidebar (categories nav-counts)
  "Render the sidebar-style buffer with CATEGORIES and NAV-COUNTS."
  (let ((inhibit-read-only t)
        (unread (or (plist-get nav-counts :unread) 0))
        (new-count (or (plist-get nav-counts :new) 0))
        (msg-count (or (plist-get nav-counts :messages) 0)))
    (erase-buffer)
    ;; --- Navigation section ---
    (insert "\n")
    (discourse-ui--insert-nav-item "  ⊙  " "Topics" 'nav-topics
                                    (when (> (+ unread new-count) 0)
                                      (format "%d unread" (+ unread new-count))))
    (discourse-ui--insert-nav-item "  ✎  " "My Posts" 'nav-my-posts nil)
    (discourse-ui--insert-nav-item "  ✉  " "My Messages" 'nav-messages
                                    (when (> msg-count 0)
                                      (format "%d" msg-count)))
    (discourse-ui--insert-nav-item "  ☆  " "Bookmarks" 'nav-bookmarks nil)
    (discourse-ui--insert-nav-item "  🔔 " "Notifications" 'nav-notifications nil)
    (discourse-ui--insert-nav-item "  🔍 " "Search" 'nav-search nil)
    ;; --- Separator ---
    (insert "\n")
    (insert (propertize "  ⌄  CATEGORIES"
                        'face '(:weight bold :inherit discourse-ui-date-face))
            "\n\n")
    ;; --- Category list ---
    (let ((sorted-cats (sort (append categories nil)
                             (lambda (a b)
                               (string< (downcase (or (alist-get 'name a) ""))
                                        (downcase (or (alist-get 'name b) "")))))))
      (dolist (cat sorted-cats)
        (discourse-ui--insert-category-item cat)))
    ;; --- Footer with keybinding hints ---
    (insert "\n")
    (insert (propertize "  RET:open  n/p:navigate  o:content  s:search  g:refresh  q:quit"
                        'face 'discourse-ui-date-face)
            "\n")))

(defun discourse-ui--insert-nav-item (icon label action &optional count-str)
  "Insert a navigation item with ICON, LABEL, ACTION type, and optional COUNT-STR."
  (let ((start (point)))
    (insert (propertize icon 'face 'discourse-ui-date-face))
    (insert (propertize label 'face '(:weight bold)))
    (when count-str
      (insert "  "
              (propertize count-str 'face '(:inherit discourse-ui-unread-face :weight bold))))
    (insert "\n")
    (put-text-property start (point) 'discourse-action action)
    (put-text-property start (point) 'discourse-item t)))

(defun discourse-ui--insert-category-item (cat)
  "Insert a single category CAT as a sidebar item with colored square."
  (let* ((id (alist-get 'id cat))
         (name (or (alist-get 'name cat) ""))
         (color (alist-get 'color cat))
         (unread (or (alist-get 'unread cat) 0))
         (new-topics (or (alist-get 'new_topics cat) 0))
         (has-activity (or (> unread 0) (> new-topics 0)))
         (color-str (discourse-ui--hex-to-color color))
         (start (point)))
    ;; Colored square indicator
    (insert "  "
            (propertize "■ " 'face `(:foreground ,color-str)))
    ;; Category name
    (insert (propertize name 'face (if has-activity
                                       'discourse-ui-unread-face
                                     'discourse-ui-category-face)))
    ;; Unread / new counts
    (when (> unread 0)
      (insert "  "
              (propertize (format "%d unread" unread)
                          'face '(:inherit discourse-ui-unread-face))))
    (when (> new-topics 0)
      (insert "  "
              (propertize (format "%d new" new-topics)
                          'face '(:foreground "#3c8dbc" :weight bold))))
    (insert "\n")
    ;; Store metadata on the region
    (put-text-property start (point) 'discourse-category-id id)
    (put-text-property start (point) 'discourse-category-slug
                       (alist-get 'slug cat))
    (put-text-property start (point) 'discourse-category-name name)
    (put-text-property start (point) 'discourse-action 'category)
    (put-text-property start (point) 'discourse-item t)))

(defun discourse-ui--compute-category-counts ()
  "Compute per-category new/unread counts from latest topics using client read state.
Returns an alist of (category-id . (:new N :unread M))."
  (let ((counts (make-hash-table :test 'eql))
        (site-url (when discourse--current-site
                    (discourse-site-url discourse--current-site))))
    (when site-url
      (condition-case nil
          (let* ((data (discourse-api-get-latest-topics nil))
                 (topic-list (alist-get 'topic_list data))
                 (topics (alist-get 'topics topic-list)))
            (seq-doseq (topic (if (vectorp topics) topics (vconcat topics)))
              (let* ((cat-id (alist-get 'category_id topic))
                     (topic-id (alist-get 'id topic))
                     (highest (or (alist-get 'highest_post_number topic) 0))
                     (read-post (discourse-topic-read-post-number site-url topic-id))
                     (entry (or (gethash cat-id counts) (list :new 0 :unread 0))))
                (cond
                 ((= read-post 0)
                  (plist-put entry :new (1+ (plist-get entry :new))))
                 ((> highest read-post)
                  (plist-put entry :unread (1+ (plist-get entry :unread)))))
                (puthash cat-id entry counts))))
        (error nil)))
    counts))

(defun discourse-ui--inject-category-counts (categories counts)
  "Inject client-side COUNTS into CATEGORIES data for sidebar display."
  (mapcar (lambda (cat)
            (let* ((id (alist-get 'id cat))
                   (entry (gethash id counts)))
              (if entry
                  (let ((cat-copy (copy-alist cat)))
                    (setf (alist-get 'new_topics cat-copy)
                          (max (or (alist-get 'new_topics cat-copy) 0)
                               (plist-get entry :new)))
                    (setf (alist-get 'unread cat-copy)
                          (max (or (alist-get 'unread cat-copy) 0)
                               (plist-get entry :unread)))
                    cat-copy)
                cat)))
          categories))

(defun discourse-ui-show-categories ()
  "Display the sidebar-style category list for the current Discourse site."
  (interactive)
  (unless discourse--current-site
    (user-error "Not connected to any Discourse site.  Run `discourse-connect' first"))
  (let* ((host (url-host (url-generic-parse-url
                           (discourse-site-url discourse--current-site))))
         (buf (get-buffer-create (format "*Discourse: %s*" host))))
    (with-current-buffer buf
      (discourse-ui-categories-mode)
      (setq header-line-format
            (concat " " (propertize host 'face 'discourse-ui-category-face)
                    "  "
                    (propertize (format "(%s)"
                                        (or (discourse-site-username discourse--current-site)
                                            "anonymous"))
                                'face 'discourse-ui-date-face)))
      (discourse-ui-refresh-categories))
    ;; Register as the sidebar buffer
    (setq discourse-ui--sidebar-buffer buf)
    ;; If sidebar is already displayed, just refresh it; otherwise show it
    (unless (discourse-ui--sidebar-window)
      (switch-to-buffer buf))))

(defun discourse-ui-refresh-categories ()
  "Refresh the sidebar category list."
  (interactive)
  (message "Fetching categories...")
  (let ((cats (discourse-api-get-categories-with-counts)))
    (if (null cats)
        ;; Fall back to basic categories endpoint
        (setq cats (discourse-api-get-categories)))
    (if (null cats)
        (message "No categories found or request failed.")
      ;; Compute client-side new/unread counts per category
      (let ((cat-counts (discourse-ui--compute-category-counts)))
        (setq cats (discourse-ui--inject-category-counts cats cat-counts)))
      (setq discourse-ui--categories-data cats)
      ;; Fetch navigation counts
      (let ((nav-counts (discourse-ui--fetch-nav-counts)))
        (setq discourse-ui--nav-items nav-counts)
        (discourse-ui--render-sidebar cats nav-counts)
        (goto-char (point-min))
        ;; Move to first actionable item
        (discourse-ui-sidebar-next-item)
        (message "Fetched %d categories." (length cats))))))

(defun discourse-ui--fetch-nav-counts ()
  "Fetch unread/new/message counts for the navigation section."
  (let ((unread 0) (new-count 0) (msg-count 0))
    ;; Try to get unread count
    (condition-case nil
        (let* ((data (discourse-api-get-unread-topics))
               (topic-list (alist-get 'topic_list data))
               (topics (alist-get 'topics topic-list)))
          (when topics (setq unread (length topics))))
      (error nil))
    ;; Try to get new topics count
    (condition-case nil
        (let* ((data (discourse-api-get-new-topics))
               (topic-list (alist-get 'topic_list data))
               (topics (alist-get 'topics topic-list)))
          (when topics (setq new-count (length topics))))
      (error nil))
    ;; Try to get message count
    (condition-case nil
        (when (discourse-site-username discourse--current-site)
          (let* ((data (discourse-api-get-private-messages
                        (discourse-site-username discourse--current-site)))
                 (topic-list (alist-get 'topic_list data))
                 (topics (alist-get 'topics topic-list)))
            (when topics (setq msg-count (length topics)))))
      (error nil))
    (list :unread unread :new new-count :messages msg-count)))

(defun discourse-ui-sidebar-next-item ()
  "Move to the next actionable item in the sidebar."
  (interactive)
  (let ((pos (next-single-property-change (point) 'discourse-item)))
    (when pos
      (goto-char pos)
      ;; If we landed between items, find the start of the next one
      (unless (get-text-property (point) 'discourse-item)
        (let ((next (next-single-property-change (point) 'discourse-item)))
          (when next (goto-char next)))))
    (beginning-of-line)))

(defun discourse-ui-sidebar-prev-item ()
  "Move to the previous actionable item in the sidebar."
  (interactive)
  (beginning-of-line)
  (let ((pos (previous-single-property-change (point) 'discourse-item)))
    (when pos
      (goto-char pos)
      (unless (get-text-property (point) 'discourse-item)
        (let ((prev (previous-single-property-change (point) 'discourse-item)))
          (when prev (goto-char prev)))))
    (beginning-of-line)))

(defun discourse-ui-activate-item ()
  "Activate the sidebar item at point."
  (interactive)
  (let ((action (get-text-property (point) 'discourse-action)))
    (pcase action
      ('category
       (let ((slug (get-text-property (point) 'discourse-category-slug))
             (id (get-text-property (point) 'discourse-category-id))
             (name (get-text-property (point) 'discourse-category-name)))
         (when (and slug id)
           (discourse-ui-show-topics slug id name))))
      ('nav-topics
       (discourse-ui-show-latest-topics))
      ('nav-my-posts
       (when (discourse-site-username discourse--current-site)
         (discourse-ui--show-user-topics
          (discourse-site-username discourse--current-site))))
      ('nav-messages
       (when (discourse-site-username discourse--current-site)
         (discourse-ui--show-private-messages
          (discourse-site-username discourse--current-site))))
      ('nav-bookmarks
       (message "Bookmarks not yet implemented."))
      ('nav-notifications
       (discourse-ui-notifications))
      ('nav-search
       (discourse-ui-search))
      (_ (message "Nothing to open at point.")))))

(defun discourse-ui--show-user-topics (username)
  "Show topics created by USERNAME."
  (message "Fetching topics by %s..." username)
  (let ((data (discourse-api-get-user-topics username)))
    (if (or (null data) (alist-get 'error data))
        (message "Failed to fetch user topics.")
      (let* ((topic-list (alist-get 'topic_list data))
             (topics (alist-get 'topics topic-list))
             (users (alist-get 'users data))
             (host (url-host (url-generic-parse-url
                               (discourse-site-url discourse--current-site))))
             (buf (get-buffer-create
                   (format "*Discourse: %s - My Posts*" host))))
        (with-current-buffer buf
          (discourse-ui-topics-mode)
          (setq discourse-ui--topics-category-slug nil
                discourse-ui--topics-category-id nil
                discourse-ui--topics-category-name (format "Posts by %s" username)
                discourse-ui--topics-page 0
                discourse-ui--topics-data topics
                discourse-ui--topics-users users)
          (setq header-line-format
                (discourse-ui--topics-header-line
                 host (format "Posts by %s" username) 0))
          (setq tabulated-list-entries
                (cl-remove-if #'null
                              (mapcar (lambda (tp)
                                        (discourse-ui--format-topic-entry tp users))
                                      (append topics nil))))
          (tabulated-list-print t)
          (goto-char (point-min))
          (discourse-ui--display-in-main buf))))))

(defun discourse-ui--show-private-messages (username)
  "Show private messages for USERNAME."
  (message "Fetching messages for %s..." username)
  (let ((data (discourse-api-get-private-messages username)))
    (if (or (null data) (alist-get 'error data))
        (message "Failed to fetch messages.")
      (let* ((topic-list (alist-get 'topic_list data))
             (topics (alist-get 'topics topic-list))
             (users (alist-get 'users data))
             (host (url-host (url-generic-parse-url
                               (discourse-site-url discourse--current-site))))
             (buf (get-buffer-create
                   (format "*Discourse: %s - Messages*" host))))
        (with-current-buffer buf
          (discourse-ui-topics-mode)
          (setq discourse-ui--topics-category-slug nil
                discourse-ui--topics-category-id nil
                discourse-ui--topics-category-name "Messages"
                discourse-ui--topics-page 0
                discourse-ui--topics-data topics
                discourse-ui--topics-users users)
          (setq header-line-format
                (discourse-ui--topics-header-line host "Messages" 0))
          (setq tabulated-list-entries
                (cl-remove-if #'null
                              (mapcar (lambda (tp)
                                        (discourse-ui--format-topic-entry tp users))
                                      (append topics nil))))
          (tabulated-list-print t)
          (goto-char (point-min))
          (discourse-ui--display-in-main buf))))))

;;; ======================================================================
;;; Topic List View
;;; ======================================================================

(defvar-local discourse-ui--topics-data nil
  "Cached topic data for the current buffer.")

(defvar-local discourse-ui--topics-category-slug nil
  "Current category slug.")

(defvar-local discourse-ui--topics-category-id nil
  "Current category ID.")

(defvar-local discourse-ui--topics-category-name nil
  "Current category name.")

(defvar-local discourse-ui--topics-page 0
  "Current page number for topic listing.")

(defvar-local discourse-ui--topics-users nil
  "User lookup data from topic listing response.")

(defvar discourse-ui-topics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'discourse-ui-open-topic)
    (define-key map (kbd "g") #'discourse-ui-refresh-topics)
    (define-key map (kbd "q") #'discourse-ui-topics-quit)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "N") #'discourse-ui-topics-next-page)
    (define-key map (kbd "P") #'discourse-ui-topics-prev-page)
    (define-key map (kbd "c") #'discourse-ui-new-topic)
    (define-key map (kbd "s") #'discourse-ui-search)
    (define-key map (kbd "o") #'discourse-ui-focus-sidebar)
    map)
  "Keymap for `discourse-ui-topics-mode'.")

(define-derived-mode discourse-ui-topics-mode tabulated-list-mode
  "Discourse-Topics"
  "Major mode for browsing Discourse forum topics."
  (setq tabulated-list-format
        [("" 1 nil)           ; pinned indicator
         ("Title" 50 t)
         ("Author" 15 t)
         ("Posts" 7 (lambda (a b) (< (string-to-number (elt (cadr a) 3))
                                      (string-to-number (elt (cadr b) 3)))))
         ("Views" 7 (lambda (a b) (< (string-to-number (elt (cadr a) 4))
                                      (string-to-number (elt (cadr b) 4)))))
         ("Activity" 8 (lambda (a b)
                         (let ((ta (get-text-property 0 'sort-key (elt (cadr a) 5)))
                               (tb (get-text-property 0 'sort-key (elt (cadr b) 5))))
                           (string< (or ta "") (or tb "")))))])
  (setq tabulated-list-sort-key nil) ; keep server order (newest first)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun discourse-ui--find-user-by-id (user-id users)
  "Find username for USER-ID in USERS vector."
  (when users
    (let ((user (seq-find (lambda (u) (= (alist-get 'id u) user-id))
                          (append users nil))))
      (when user
        (alist-get 'username user)))))

(defun discourse-ui--format-topic-entry (topic users)
  "Format a TOPIC alist as a tabulated-list entry, using USERS for lookup."
  (let* ((id (alist-get 'id topic))
         (title (or (alist-get 'title topic) ""))
         (pinned (alist-get 'pinned topic))
         (posts-count (or (alist-get 'posts_count topic)
                          (1+ (or (alist-get 'reply_count topic) 0))))
         (views (or (alist-get 'views topic) 0))
         (last-posted (or (alist-get 'last_posted_at topic)
                          (alist-get 'created_at topic) ""))
         (highest-post (or (alist-get 'highest_post_number topic) 0))
         ;; Server-side tracking
         (unseen (eq (alist-get 'unseen topic) t))
         (unread-posts (or (alist-get 'unread_posts topic) 0))
         (new-posts (or (alist-get 'new_posts topic) 0))
         (server-unread (or unseen (> unread-posts 0) (> new-posts 0)))
         ;; Client-side tracking (fallback)
         (site-url (when discourse--current-site
                     (discourse-site-url discourse--current-site)))
         (read-post (if site-url
                        (discourse-topic-read-post-number site-url id)
                      0))
         (client-new (= read-post 0))
         (client-unread (and (> read-post 0) (> highest-post read-post)))
         ;; Merge: server wins when it has data, else use client
         (is-new (or unseen client-new))
         (is-unread (or server-unread client-unread))
         (unread-count (if server-unread
                           (+ unread-posts new-posts)
                         (if client-unread
                             (- highest-post read-post)
                           0)))
         (title-display (cond
                         ((and is-new client-new)
                          (format "%s  [NEW]" title))
                         ((and is-unread (> unread-count 0))
                          (format "%s  (%d new)" title unread-count))
                         (is-unread title)
                         (t title)))
         (title-face (cond (is-unread 'discourse-ui-unread-face)
                           (t 'discourse-ui-read-face)))
         (posters (alist-get 'posters topic))
         (author-name
          (or (when posters
                (let* ((original (seq-find
                                  (lambda (p)
                                    (string-match-p "Original"
                                                    (or (alist-get 'description p) "")))
                                  (append posters nil)))
                       (user-id (and original (alist-get 'user_id original))))
                  (when user-id
                    (discourse-ui--find-user-by-id user-id users))))
              "")))
    (list id
          (vector
           (cond (pinned (propertize "*" 'face 'discourse-ui-pinned-face))
                 (is-unread (propertize "●" 'face 'discourse-ui-unread-face))
                 (t " "))
           (propertize title-display 'face title-face)
           (propertize author-name 'face (if is-unread
                                             'discourse-ui-unread-face
                                           'discourse-ui-date-face))
           (number-to-string posts-count)
           (number-to-string views)
           (propertize (discourse-ui--format-time-elapsed last-posted)
                       'face 'discourse-ui-date-face
                       'sort-key last-posted)))))

(defun discourse-ui--topics-header-line (site-host category-name page)
  "Build a header-line string for SITE-HOST, CATEGORY-NAME, and PAGE number."
  (concat " "
          (propertize site-host 'face 'discourse-ui-category-face)
          "  ›  "
          (propertize (or category-name "Topics") 'face '(:weight bold))
          "  "
          (propertize (format "[Page %d]" (1+ page)) 'face 'discourse-ui-date-face)
          "    "
          (propertize "RET:open  c:new  N/P:page  o:sidebar  s:search  g:refresh  q:back"
                      'face 'discourse-ui-date-face)))

(defun discourse-ui-show-topics (slug category-id category-name &optional page)
  "Display topics for category SLUG (CATEGORY-ID, CATEGORY-NAME) at PAGE."
  (let* ((host (url-host (url-generic-parse-url
                           (discourse-site-url discourse--current-site))))
         (buf (get-buffer-create
               (format "*Discourse: %s - %s*" host category-name))))
    (with-current-buffer buf
      (discourse-ui-topics-mode)
      (setq discourse-ui--topics-category-slug slug
            discourse-ui--topics-category-id category-id
            discourse-ui--topics-category-name category-name
            discourse-ui--topics-page (or page 0))
      (setq header-line-format
            (discourse-ui--topics-header-line host category-name discourse-ui--topics-page))
      (discourse-ui-refresh-topics)
      (discourse-ui--display-in-main buf))))

(defun discourse-ui-show-latest-topics (&optional page)
  "Display latest topics across all categories at PAGE."
  (let* ((host (url-host (url-generic-parse-url
                           (discourse-site-url discourse--current-site))))
         (buf (get-buffer-create
               (format "*Discourse: %s - Latest*" host))))
    (with-current-buffer buf
      (discourse-ui-topics-mode)
      (setq discourse-ui--topics-category-slug nil
            discourse-ui--topics-category-id nil
            discourse-ui--topics-category-name "Latest"
            discourse-ui--topics-page (or page 0))
      (setq header-line-format
            (discourse-ui--topics-header-line host "Latest" discourse-ui--topics-page))
      (discourse-ui-refresh-topics)
      (discourse-ui--display-in-main buf))))

(defun discourse-ui--update-topics-header ()
  "Update the header line to reflect the current page."
  (when discourse--current-site
    (let ((host (url-host (url-generic-parse-url
                            (discourse-site-url discourse--current-site)))))
      (setq header-line-format
            (discourse-ui--topics-header-line
             host discourse-ui--topics-category-name discourse-ui--topics-page)))))

(defun discourse-ui-refresh-topics ()
  "Refresh the topic list."
  (interactive)
  (message "Fetching topics (page %d)..." discourse-ui--topics-page)
  (let* ((data (if discourse-ui--topics-category-slug
                   (discourse-api-get-category-topics
                    discourse-ui--topics-category-slug
                    discourse-ui--topics-category-id
                    discourse-ui--topics-page)
                 (discourse-api-get-latest-topics
                  discourse-ui--topics-page)))
         (topic-list (alist-get 'topic_list data))
         (topics (alist-get 'topics topic-list))
         (users (alist-get 'users data)))
    (if (or (null topics) (= 0 (length topics)))
        (progn
          (message "No topics on page %d." (1+ discourse-ui--topics-page))
          ;; Go back if we went past the last page
          (when (> discourse-ui--topics-page 0)
            (cl-decf discourse-ui--topics-page)
            (discourse-ui--update-topics-header)))
      (setq discourse-ui--topics-data topics
            discourse-ui--topics-users users)
      (setq tabulated-list-entries
            (cl-remove-if #'null
                          (mapcar (lambda (tp) (discourse-ui--format-topic-entry tp users))
                                  (append topics nil))))
      (tabulated-list-print t)
      (goto-char (point-min))
      (discourse-ui--update-topics-header)
      (message "Page %d  (%d topics)  —  N: next page, P: prev page"
               (1+ discourse-ui--topics-page) (length topics)))))

(defun discourse-ui-topics-next-page ()
  "Go to the next page of topics."
  (interactive)
  (cl-incf discourse-ui--topics-page)
  (discourse-ui-refresh-topics))

(defun discourse-ui-topics-prev-page ()
  "Go to the previous page of topics."
  (interactive)
  (when (> discourse-ui--topics-page 0)
    (cl-decf discourse-ui--topics-page)
    (discourse-ui-refresh-topics)))

(defun discourse-ui-topics-quit ()
  "Quit topic list and return to sidebar."
  (interactive)
  (let ((buf (current-buffer)))
    ;; Ensure sidebar is visible before killing
    (let ((sidebar-win (or (discourse-ui--sidebar-window)
                           (discourse-ui--ensure-sidebar))))
      (when sidebar-win
        (select-window sidebar-win)))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun discourse-ui-open-topic ()
  "Open the topic at point, showing its posts."
  (interactive)
  (let ((topic-id (tabulated-list-get-id)))
    (when topic-id
      (discourse-ui-show-topic topic-id))))

(defun discourse-ui-latest-topics ()
  "Show latest topics across all categories."
  (interactive)
  (discourse-ui-show-latest-topics))

;;; ======================================================================
;;; Topic/Post Thread View
;;; ======================================================================

(defvar-local discourse-ui--topic-data nil
  "Full topic data for the current buffer.")

(defvar-local discourse-ui--topic-id nil
  "Topic ID for the current buffer.")

(defvar discourse-ui-topic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'discourse-ui-topic-quit)
    (define-key map (kbd "g") #'discourse-ui-refresh-topic)
    (define-key map (kbd "r") #'discourse-ui-reply-to-topic)
    (define-key map (kbd "R") #'discourse-ui-reply-to-post)
    (define-key map (kbd "l") #'discourse-ui-like-post-at-point)
    (define-key map (kbd "n") #'discourse-ui-next-post)
    (define-key map (kbd "p") #'discourse-ui-prev-post)
    (define-key map (kbd "b") #'discourse-ui-open-in-browser)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)
    (define-key map (kbd "o") #'discourse-ui-focus-sidebar)
    map)
  "Keymap for `discourse-ui-topic-mode'.")

(define-derived-mode discourse-ui-topic-mode special-mode
  "Discourse-Topic"
  "Major mode for viewing a Discourse topic thread.")

(defun discourse-ui-show-topic (topic-id)
  "Fetch and display TOPIC-ID in a thread view buffer."
  (message "Fetching topic %d..." topic-id)
  (let ((data (discourse-api-get-topic topic-id)))
    (if (or (null data) (alist-get 'error data))
        (message "Failed to fetch topic %d" topic-id)
      (discourse-mark-topic-read
       topic-id (or (alist-get 'highest_post_number data) 0))
      (let* ((title (alist-get 'title data))
             (host (when discourse--current-site
                     (url-host (url-generic-parse-url
                                (discourse-site-url discourse--current-site)))))
             (buf (get-buffer-create
                   (format "*Discourse: %s*"
                           (truncate-string-to-width (or title "Topic") 50)))))
        (with-current-buffer buf
          (discourse-ui-topic-mode)
          (setq discourse-ui--topic-data data
                discourse-ui--topic-id topic-id)
          (setq header-line-format
                (concat " " (propertize (or host "discourse") 'face 'discourse-ui-category-face)
                        "  ›  "
                        (propertize (truncate-string-to-width (or title "Topic") 50)
                                    'face '(:weight bold))
                        "    "
                        (propertize "r:reply  R:reply-to-post  n/p:post  l:like  o:sidebar  b:browser  g:refresh  q:quit"
                                    'face 'discourse-ui-date-face)))
          (discourse-ui--render-topic data)
          (goto-char (point-min))
          (discourse-ui--display-in-main buf))))))

(defun discourse-ui--render-topic (data)
  "Render topic DATA into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let* ((title (alist-get 'title data))
           (post-stream (alist-get 'post_stream data))
           (posts (alist-get 'posts post-stream))
           (views (or (alist-get 'views data) 0))
           (posts-count (or (alist-get 'posts_count data)
                            (1+ (or (alist-get 'reply_count data) 0))))
           (created (alist-get 'created_at data))
           (tags (alist-get 'tags data)))
      ;; Title header
      (insert (propertize (or title "Untitled")
                          'face '(:inherit discourse-ui-topic-title-face :height 1.3))
              "\n")
      ;; Metadata line
      (insert (propertize
               (format "%s  |  %d views  |  %d posts"
                       (discourse-ui--format-time-elapsed created)
                       views posts-count)
               'face 'discourse-ui-date-face))
      (when (and tags (> (length tags) 0))
        (insert "  |  tags: "
                (mapconcat (lambda (tag)
                             (if (stringp tag) tag
                               (or (alist-get 'name tag)
                                   (format "%S" tag))))
                           (append tags nil) ", ")))
      (insert "\n")
      (insert (propertize (make-string (min 80 (window-width)) ?─)
                          'face 'discourse-ui-post-separator-face)
              "\n\n")
      ;; Posts
      (when posts
        (seq-doseq (post (if (vectorp posts) posts (vconcat posts)))
          (discourse-ui--render-post post))))))

(defun discourse-ui--render-post (post)
  "Render a single POST alist into the current buffer."
  (let* ((username (or (alist-get 'username post) "unknown"))
         (name (alist-get 'name post))
         (created (alist-get 'created_at post))
         (cooked (alist-get 'cooked post))
         (post-number (alist-get 'post_number post))
         (post-id (alist-get 'id post))
         (likes (or (alist-get 'like_count post) 0))
         (reply-to (alist-get 'reply_to_post_number post))
         (start (point)))
    ;; Post header
    (insert (propertize (format "#%d " post-number)
                        'face 'discourse-ui-date-face)
            (propertize username 'face 'discourse-ui-author-face))
    (when (and name (not (string= name username)) (not (string= name "")))
      (insert " (" name ")"))
    (insert "  "
            (propertize (discourse-ui--format-time-elapsed created)
                        'face 'discourse-ui-date-face))
    (when (and reply-to (> reply-to 0))
      (insert (propertize (format "  (reply to #%d)" reply-to)
                          'face 'discourse-ui-date-face)))
    (when (> likes 0)
      (insert "  "
              (propertize (format "♥ %d" likes) 'face 'discourse-ui-likes-face)))
    (insert "\n")
    ;; Post body
    (when cooked
      (if discourse-ui-render-html
          (let ((shr-width (min (- (window-width) 4) 80))
                (shr-use-fonts nil)
                (shr-inhibit-images nil)
                (shr-base (when discourse--current-site
                            (url-generic-parse-url
                             (discourse-site-url discourse--current-site))))
                (dom (with-temp-buffer
                       (insert (discourse-ui--massage-html cooked))
                       (libxml-parse-html-region (point-min) (point-max)))))
            (shr-insert-document dom))
        (insert (discourse-ui--massage-html cooked))))
    (insert "\n")
    ;; Separator
    (insert (propertize (make-string (min 80 (window-width)) ?─)
                        'face 'discourse-ui-post-separator-face)
            "\n\n")
    ;; Store post metadata as text properties on the region
    (put-text-property start (point) 'discourse-post-id post-id)
    (put-text-property start (point) 'discourse-post-number post-number)
    (put-text-property start (point) 'discourse-post-username username)))

;;; --- Topic view commands ---

(defun discourse-ui-topic-quit ()
  "Quit topic view and return to previous window or sidebar."
  (interactive)
  (let ((buf (current-buffer)))
    ;; Try to find another non-sidebar window to land on
    (let* ((sidebar-win (discourse-ui--sidebar-window))
           (other-win (seq-find (lambda (w)
                                  (and (not (eq w (selected-window)))
                                       (not (eq w sidebar-win))
                                       (not (window-minibuffer-p w))))
                                (window-list))))
      (cond
       (other-win (select-window other-win))
       (sidebar-win (select-window sidebar-win))
       ((discourse-ui--ensure-sidebar)
        (select-window (discourse-ui--ensure-sidebar)))))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun discourse-ui-refresh-topic ()
  "Refresh the current topic view."
  (interactive)
  (when discourse-ui--topic-id
    (let ((data (discourse-api-get-topic discourse-ui--topic-id)))
      (when data
        (setq discourse-ui--topic-data data)
        (discourse-ui--render-topic data)
        (goto-char (point-min))))))

(defun discourse-ui-next-post ()
  "Move to the next post in the topic view."
  (interactive)
  (let ((current-post (get-text-property (point) 'discourse-post-number)))
    (if current-post
        (let ((next-pos (next-single-property-change (point) 'discourse-post-number)))
          (when next-pos
            (goto-char next-pos)
            ;; Skip to start of next post's content
            (when (next-single-property-change next-pos 'discourse-post-number)
              (goto-char next-pos))))
      (goto-char (next-single-property-change (point) 'discourse-post-number
                                               nil (point-max))))))

(defun discourse-ui-prev-post ()
  "Move to the previous post in the topic view."
  (interactive)
  (let ((prev-pos (previous-single-property-change (point) 'discourse-post-number)))
    (when prev-pos
      (goto-char (previous-single-property-change prev-pos 'discourse-post-number
                                                   nil (point-min))))))

(defun discourse-ui--post-at-point ()
  "Return (post-id . post-number) at point, or nil."
  (let ((id (get-text-property (point) 'discourse-post-id))
        (num (get-text-property (point) 'discourse-post-number)))
    (when (and id num)
      (cons id num))))

(defun discourse-ui-like-post-at-point ()
  "Like the post at point."
  (interactive)
  (if-let* ((info (discourse-ui--post-at-point))
            (post-id (car info)))
      (progn
        (discourse-api-like-post post-id)
        (message "Liked post #%d" (cdr info)))
    (message "No post at point.")))

(defun discourse-ui-reply-to-topic ()
  "Reply to the current topic (not to a specific post)."
  (interactive)
  (when discourse-ui--topic-id
    (let ((title (alist-get 'title discourse-ui--topic-data)))
      (discourse-compose--reply discourse-ui--topic-id nil title))))

(defun discourse-ui-reply-to-post ()
  "Reply to the specific post at point."
  (interactive)
  (when discourse-ui--topic-id
    (if-let* ((info (discourse-ui--post-at-point)))
        (let ((title (alist-get 'title discourse-ui--topic-data))
              (username (get-text-property (point) 'discourse-post-username)))
          (discourse-compose--reply discourse-ui--topic-id (cdr info)
                                    title username))
      (message "No post at point."))))

(defun discourse-ui-open-in-browser ()
  "Open the current topic in a web browser."
  (interactive)
  (when (and discourse--current-site discourse-ui--topic-id)
    (let* ((slug (or (alist-get 'slug discourse-ui--topic-data) ""))
           (url (format "%s/t/%s/%d"
                        (discourse-site-url discourse--current-site)
                        slug discourse-ui--topic-id)))
      (browse-url url))))

;;; --- Search ---

(defun discourse-ui-search ()
  "Search the current Discourse site."
  (interactive)
  (let ((query (read-string "Search Discourse: ")))
    (when (and query (not (string-empty-p query)))
      (message "Searching for '%s'..." query)
      (let ((results (discourse-api-search query)))
        (if (or (null results) (alist-get 'error results))
            (message "Search failed or no results.")
          (discourse-ui--show-search-results query results))))))

(defun discourse-ui--show-search-results (query data)
  "Display search results for QUERY from DATA."
  (let* ((topics (alist-get 'topics data))
         (posts (alist-get 'posts data))
         (buf (get-buffer-create
               (format "*Discourse: Search '%s'*"
                       (truncate-string-to-width query 30)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (discourse-ui-topic-mode)
        (insert (propertize (format "Search results for: %s\n\n" query)
                            'face '(:weight bold :height 1.2)))
        (when topics
          (insert (propertize "Topics:\n" 'face '(:weight bold)) "\n")
          (seq-doseq (topic (if (vectorp topics) topics (vconcat topics)))
            (let ((id (alist-get 'id topic))
                  (title (or (alist-get 'title topic) "")))
              (insert-text-button title
                                  'action (lambda (_) (discourse-ui-show-topic id))
                                  'face 'discourse-ui-topic-title-face)
              (insert "\n")))
          (insert "\n"))
        (when posts
          (insert (propertize "Posts:\n" 'face '(:weight bold)) "\n")
          (seq-doseq (post (if (vectorp posts) posts (vconcat posts)))
            (let ((topic-id (alist-get 'topic_id post))
                  (blurb (or (alist-get 'blurb post) ""))
                  (username (or (alist-get 'username post) "")))
              (insert (propertize username 'face 'discourse-ui-author-face) ": ")
              (insert-text-button (truncate-string-to-width blurb 80 nil nil t)
                                  'action (lambda (_)
                                            (discourse-ui-show-topic topic-id))
                                  'face 'default)
              (insert "\n")))))
      (goto-char (point-min))
      (discourse-ui--display-in-main buf))))

;;; --- Notifications ---

(defun discourse-ui-notifications ()
  "Show notifications for the current user."
  (interactive)
  (message "Fetching notifications...")
  (let ((data (discourse-api-get-notifications)))
    (if (or (null data) (alist-get 'error data))
        (message "Failed to fetch notifications.")
      (discourse-ui--show-notifications data))))

(defun discourse-ui--notification-type-name (type-id)
  "Return a human-readable name for notification TYPE-ID."
  (pcase type-id
    (1 "mentioned")
    (2 "replied")
    (3 "quoted")
    (4 "edited")
    (5 "liked")
    (6 "private message")
    (7 "invited to topic")
    (8 "invitee accepted")
    (9 "posted")
    (10 "moved post")
    (11 "linked")
    (12 "granted badge")
    (13 "invited to PM")
    (14 "watching first post")
    (15 "topic reminder")
    (16 "liked consolidated")
    (17 "post approved")
    (_ (format "type:%d" type-id))))

(defun discourse-ui--show-notifications (data)
  "Display notifications from DATA."
  (let* ((notifications (alist-get 'notifications data))
         (buf (get-buffer-create "*Discourse: Notifications*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (discourse-ui-topic-mode)
        (insert (propertize "Notifications\n\n" 'face '(:weight bold :height 1.3)))
        (if (or (null notifications) (= 0 (length notifications)))
            (insert "No notifications.\n")
          (seq-doseq (notif (if (vectorp notifications)
                                notifications
                              (vconcat notifications)))
            (let* ((type (alist-get 'notification_type notif))
                   (read-p (alist-get 'read notif))
                   (data (alist-get 'data notif))
                   (topic-title (or (alist-get 'topic_title data) ""))
                   (display-name (or (alist-get 'display_username data) ""))
                   (topic-id (alist-get 'topic_id notif))
                   (created (alist-get 'created_at notif)))
              (insert (if (eq read-p :json-false) "● " "  "))
              (insert (propertize (discourse-ui--notification-type-name type)
                                  'face 'discourse-ui-category-face)
                      " ")
              (when (not (string-empty-p display-name))
                (insert (propertize display-name 'face 'discourse-ui-author-face)
                        " "))
              (when (and topic-id topic-title)
                (insert-text-button topic-title
                                    'action (lambda (_)
                                              (discourse-ui-show-topic topic-id))
                                    'face 'discourse-ui-topic-title-face))
              (insert "  "
                      (propertize (discourse-ui--format-time-elapsed created)
                                  'face 'discourse-ui-date-face)
                      "\n")))))
      (goto-char (point-min))
      (discourse-ui--display-in-main buf))))

;;; --- New topic from topic list ---

(defun discourse-ui-new-topic ()
  "Compose a new topic in the current category."
  (interactive)
  (if discourse-ui--topics-category-id
      (discourse-compose--new-topic discourse-ui--topics-category-id
                                     discourse-ui--topics-category-name)
    (message "No category context.  Navigate to a category first.")))

(provide 'discourse-ui)

;;; discourse-ui.el ends here
