;;; discourse-compose.el --- Compose new topics and replies  -*- lexical-binding: t; coding: utf-8 -*-

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

;; Compose buffer for creating new Discourse topics and replying to posts.
;; Discourse uses Markdown for post content.

;;; Code:

(require 'cl-lib)
(require 'discourse-api)

(declare-function discourse-ui-show-topic "discourse-ui" (topic-id))

;;; --- Customization ---

(defgroup discourse-compose nil
  "Discourse compose settings."
  :group 'discourse
  :prefix "discourse-compose-")

(defcustom discourse-compose-use-markdown-mode t
  "If non-nil and `markdown-mode' is available, use it for compose buffers."
  :type 'boolean
  :group 'discourse-compose)

(defcustom discourse-compose-fill-column 72
  "Fill column for compose buffers."
  :type 'integer
  :group 'discourse-compose)

;;; --- Buffer-local state ---

(defvar-local discourse-compose--type nil
  "Type of compose: `new-topic' or `reply'.")

(defvar-local discourse-compose--category-id nil
  "Category ID for new topics.")

(defvar-local discourse-compose--category-name nil
  "Category name for new topics.")

(defvar-local discourse-compose--topic-id nil
  "Topic ID for replies.")

(defvar-local discourse-compose--reply-to-post-number nil
  "Post number being replied to, or nil for topic-level reply.")

(defvar-local discourse-compose--title nil
  "Title for new topics (stored in header), or topic title for replies.")

;;; --- Compose mode ---

(defvar discourse-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'discourse-compose-send)
    (define-key map (kbd "C-c C-k") #'discourse-compose-cancel)
    (define-key map (kbd "C-c C-p") #'discourse-compose-preview)
    (define-key map (kbd "C-c C-i") #'discourse-compose-attach-file)
    map)
  "Keymap for `discourse-compose-mode'.")

(define-derived-mode discourse-compose-mode text-mode "Discourse-Compose"
  "Major mode for composing Discourse posts.
Discourse uses Markdown for formatting.

\\{discourse-compose-mode-map}"
  (setq-local fill-column discourse-compose-fill-column)
  ;; Try to activate markdown-mode features if available
  (when (and discourse-compose-use-markdown-mode
             (fboundp 'markdown-mode))
    (markdown-mode)
    ;; Re-apply our keymap on top
    (use-local-map (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map (current-local-map))
                     (define-key map (kbd "C-c C-c") #'discourse-compose-send)
                     (define-key map (kbd "C-c C-k") #'discourse-compose-cancel)
                     (define-key map (kbd "C-c C-p") #'discourse-compose-preview)
                     (define-key map (kbd "C-c C-i") #'discourse-compose-attach-file)
                     map)))
  ;; @mention username completion
  (add-hook 'completion-at-point-functions
            #'discourse-compose--mention-capf nil t))

;;; --- Entry points ---

(defun discourse-compose--new-topic (category-id &optional category-name)
  "Open a compose buffer for a new topic in CATEGORY-ID (CATEGORY-NAME)."
  (let ((buf (get-buffer-create
              (format "*Discourse: New Topic in %s*"
                      (or category-name (format "cat:%d" category-id))))))
    (with-current-buffer buf
      (discourse-compose-mode)
      (setq discourse-compose--type 'new-topic
            discourse-compose--category-id category-id
            discourse-compose--category-name category-name)
      (erase-buffer)
      (insert "Title: \n")
      (insert (make-string 40 ?─) "\n")
      (insert "<!-- Write your topic body below in Markdown -->\n")
      (insert "<!-- C-c C-c to send, C-c C-k to cancel, C-c C-i to attach file -->\n\n")
      (goto-char (point-min))
      (end-of-line))
    (switch-to-buffer buf)))

(defun discourse-compose--reply (topic-id &optional reply-to-post-number
                                          topic-title reply-to-username)
  "Open a compose buffer for a reply in TOPIC-ID.
If REPLY-TO-POST-NUMBER is non-nil, this is a reply to that specific post.
TOPIC-TITLE and REPLY-TO-USERNAME are used for display."
  (let* ((label (if reply-to-post-number
                    (format "Reply to %s #%d"
                            (or reply-to-username "post")
                            reply-to-post-number)
                  (format "Reply to: %s" (or topic-title "topic"))))
         (buf (get-buffer-create (format "*Discourse: %s*" label))))
    (with-current-buffer buf
      (discourse-compose-mode)
      (setq discourse-compose--type 'reply
            discourse-compose--topic-id topic-id
            discourse-compose--reply-to-post-number reply-to-post-number
            discourse-compose--title topic-title)
      (erase-buffer)
      (insert (format "<!-- %s -->\n" label))
      (insert "<!-- C-c C-c to send, C-c C-k to cancel, C-c C-i to attach file -->\n\n")
      (goto-char (point-max)))
    (switch-to-buffer buf)))

;;; --- Send / Cancel ---

(defun discourse-compose--extract-title ()
  "Extract the title from the compose buffer header line.
Returns the title string or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Title:\\s-*\\(.*\\)$" nil t)
      (let ((title (string-trim (match-string 1))))
        (when (> (length title) 0) title)))))

(defun discourse-compose--extract-body ()
  "Extract the body text from the compose buffer.
Strips the header/separator lines for new topics."
  (save-excursion
    (goto-char (point-min))
    (let ((body-start (point-min)))
      ;; For new topics, skip past the Title: line and separator
      (when (eq discourse-compose--type 'new-topic)
        (when (re-search-forward "^─+" nil t)
          (forward-line 1)
          (setq body-start (point))))
      ;; Strip HTML comment lines at the top
      (goto-char body-start)
      (while (and (not (eobp))
                  (looking-at "^\\s-*<!--.*-->\\s-*$"))
        (forward-line 1)
        (setq body-start (point)))
      ;; Skip leading blank lines
      (goto-char body-start)
      (while (and (not (eobp)) (looking-at "^\\s-*$"))
        (forward-line 1)
        (setq body-start (point)))
      (string-trim (buffer-substring-no-properties body-start (point-max))))))

(defun discourse-compose-send ()
  "Send the composed message."
  (interactive)
  (let ((body (discourse-compose--extract-body)))
    (when (string-empty-p body)
      (user-error "Cannot send empty post"))
    (cl-case discourse-compose--type
      (new-topic
       (let ((title (discourse-compose--extract-title)))
         (unless title
           (user-error "New topic requires a title"))
         (when (yes-or-no-p (format "Create topic \"%s\" in %s? "
                                    title
                                    (or discourse-compose--category-name "this category")))
           (message "Creating topic...")
           (let ((result (discourse-api-create-topic
                          title body discourse-compose--category-id)))
             (if (and result (not (alist-get 'error result)))
                 (progn
                   (message "Topic created successfully!")
                   (let ((topic-id (alist-get 'topic_id result)))
                     (kill-buffer (current-buffer))
                     (when topic-id
                       (discourse-ui-show-topic topic-id))))
               (message "Failed to create topic: %S"
                        (or (alist-get 'errors result)
                            (alist-get 'body result)
                            result)))))))
      (reply
       (when (yes-or-no-p "Send reply? ")
         (message "Sending reply...")
         (let ((result (discourse-api-create-reply
                        discourse-compose--topic-id
                        body
                        discourse-compose--reply-to-post-number)))
           (if (and result (not (alist-get 'error result)))
               (progn
                 (message "Reply sent successfully!")
                 (let ((topic-id discourse-compose--topic-id))
                   (kill-buffer (current-buffer))
                   (discourse-ui-show-topic topic-id)))
             (message "Failed to send reply: %S"
                      (or (alist-get 'errors result)
                          (alist-get 'body result)
                          result))))))
      (t (user-error "Unknown compose type: %s" discourse-compose--type)))))

(defun discourse-compose-cancel ()
  "Cancel composing and kill the compose buffer."
  (interactive)
  (when (yes-or-no-p "Discard this draft? ")
    (kill-buffer (current-buffer))))

(defun discourse-compose-preview ()
  "Preview the current compose buffer body (basic)."
  (interactive)
  (let ((body (discourse-compose--extract-body))
        (title (when (eq discourse-compose--type 'new-topic)
                 (discourse-compose--extract-title))))
    (with-current-buffer (get-buffer-create "*Discourse: Preview*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when title
          (insert (propertize title 'face '(:weight bold :height 1.2)) "\n\n"))
        (insert body)
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

;;; --- @mention username completion ---

(defvar-local discourse-compose--user-cache nil
  "Cache of (TIMESTAMP . USERS-ALIST) for mention completion.")

(defvar discourse-compose--user-cache-ttl 30
  "Seconds to cache user search results.")

(defun discourse-compose--search-users (term)
  "Search for users matching TERM, with caching."
  (let* ((now (float-time))
         (cached discourse-compose--user-cache)
         (cache-key (downcase (or term ""))))
    ;; Return cached if same prefix and fresh
    (if (and cached
             (string-prefix-p (car cached) cache-key)
             (< (- now (cadr cached)) discourse-compose--user-cache-ttl))
        (cddr cached)
      (let ((users (discourse-api-search-users term)))
        (setq discourse-compose--user-cache
              (cons cache-key (cons now users)))
        users))))

(defun discourse-compose--mention-capf ()
  "Completion-at-point function for @mentions in compose buffers."
  (when-let* ((end (point))
              (start (save-excursion
                       (when (re-search-backward "@\\([A-Za-z0-9_.-]*\\)\\="
                                                 (line-beginning-position) t)
                         (1+ (match-beginning 0))))))
    (let ((prefix (buffer-substring-no-properties start end)))
      (list start end
            (completion-table-dynamic
             (lambda (_)
               (mapcar (lambda (u) (alist-get 'username u))
                       (discourse-compose--search-users prefix))))
            :annotation-function
            (lambda (candidate)
              (when-let* ((users (cddr discourse-compose--user-cache))
                          (user (seq-find
                                 (lambda (u)
                                   (string= (alist-get 'username u) candidate))
                                 users))
                          (name (alist-get 'name user)))
                (when (and name (not (string= name "")))
                  (format "  (%s)" name))))
            :exclusive 'no))))

;;; --- File/Image attachments ---

(defun discourse-compose--image-p (filename)
  "Return non-nil if FILENAME looks like an image."
  (let ((ext (downcase (or (file-name-extension filename) ""))))
    (member ext '("jpg" "jpeg" "png" "gif" "webp" "svg" "bmp" "ico"))))

(defun discourse-compose-attach-file (file-path)
  "Upload FILE-PATH to Discourse and insert a markdown link at point.
For images, inserts ![name](url).  For other files, inserts [name](url).
Prompts for a file path interactively."
  (interactive
   (list (read-file-name "Attach file: " nil nil t)))
  (unless (file-exists-p file-path)
    (user-error "File does not exist: %s" file-path))
  (unless discourse--current-site
    (user-error "Not connected to any Discourse site"))
  (let ((filename (file-name-nondirectory file-path))
        (size (file-attribute-size (file-attributes file-path))))
    (message "Uploading %s (%s)..." filename
             (file-size-human-readable size))
    (let ((result (discourse-api-upload-file file-path)))
      (if (null result)
          (user-error "Upload failed — check *Messages* for details")
        (let* ((short-url (alist-get 'short_url result))
               (full-url (alist-get 'url result))
               (base-url (when discourse--current-site
                           (discourse-site-url discourse--current-site)))
               ;; Build a proper absolute URL from the response
               (url-path (cond
                          ;; Protocol-relative URL like //host:port/path
                          ;; Extract just the path part after the host
                          ((and full-url (string-prefix-p "//" full-url))
                           (if (string-match "//[^/]+\\(/.*\\)" full-url)
                               (match-string 1 full-url)
                             full-url))
                          (t full-url)))
               (url (cond
                     ;; Combine path with actual base URL
                     ((and url-path base-url (string-prefix-p "/" url-path))
                      (concat base-url url-path))
                     ;; Already absolute
                     ((and url-path (string-match-p "^https?://" url-path))
                      url-path)
                     ;; Fallback to short_url
                     (t short-url)))
               (orig-name (or (alist-get 'original_filename result) filename))
               (width (alist-get 'width result))
               (height (alist-get 'height result))
               (is-image (discourse-compose--image-p orig-name))
               (markdown
                (if is-image
                    (if (and width height)
                        (format "![%s|%dx%d](%s)" orig-name width height url)
                      (format "![%s](%s)" orig-name url))
                  (format "[%s|attachment](%s)" orig-name url))))
          (insert markdown)
          (message "Attached: %s" orig-name))))))

(provide 'discourse-compose)

;;; discourse-compose.el ends here
