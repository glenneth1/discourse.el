;;; discourse.el --- Browse and post to Discourse forums from Emacs  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2024 The Authors of discourse.el

;; Authors: Glenn <glenn>
;; Version: 0.1.0
;; Keywords: comm, forum
;; URL: https://github.com/glenneth1/discourse.el
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

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

;; discourse.el is a standalone Emacs package for browsing and interacting
;; with Discourse forums.  Unlike nndiscourse (a Gnus backend), this package
;; provides its own buffer-based UI without depending on Gnus.
;;
;; Features:
;;   - Browse categories and topics
;;   - Read topic threads with rendered HTML posts
;;   - Create new topics and reply to posts
;;   - Like posts
;;   - Search forums
;;   - View notifications
;;
;; Authentication:
;;   Credentials are stored in ~/.authinfo.gpg (via auth-source).
;;   Supports both API key and session-based (username/password) auth.
;;
;;   For API key auth, add to your .authinfo.gpg:
;;     machine discourse.example.com login YOUR_USERNAME password YOUR_API_KEY
;;
;;   For session auth (username/password), same format:
;;     machine discourse.example.com login YOUR_USERNAME password YOUR_PASSWORD
;;
;;   Set `discourse-api-auth-method' to control which method is used.
;;
;; Quick start:
;;   M-x discourse-connect RET https://discourse.example.com RET
;;
;; Or configure a default site:
;;   (setq discourse-default-url "https://discourse.example.com")
;;   M-x discourse

;;; Code:

(require 'discourse-api)
(require 'discourse-ui)
(require 'discourse-compose)

;;; --- Customization ---

(defgroup discourse nil
  "Browse and post to Discourse forums from Emacs."
  :group 'comm
  :prefix "discourse-")

(defcustom discourse-default-url nil
  "Default Discourse forum URL.
When set, `discourse' connects to this site automatically."
  :type '(choice (const :tag "None (always prompt)" nil)
                 (string :tag "URL"))
  :group 'discourse)

(defcustom discourse-saved-sites nil
  "List of saved Discourse site URLs for quick switching."
  :type '(repeat string)
  :group 'discourse)

(defcustom discourse-site-configs nil
  "Per-site configuration alist.
Each entry is (URL . PLIST) where PLIST can contain:
  :auth-method  - `api-key', `session', or `auto' (overrides global)
Example:
  \\='((\"https://forum.example.com\" :auth-method session)
    (\"https://meta.discourse.org\" :auth-method api-key))"
  :type '(alist :key-type string :value-type plist)
  :group 'discourse)

;;; --- Interactive commands ---

(defun discourse--site-auth-method (url)
  "Return the auth method for URL, checking per-site config first."
  (let ((config (cdr (assoc url discourse-site-configs))))
    (or (plist-get config :auth-method)
        discourse-api-auth-method)))

;;;###autoload
(defun discourse-connect (url)
  "Connect to a Discourse forum at URL and show categories.
With prefix arg, always prompt for URL even if `discourse-default-url' is set."
  (interactive
   (list (or (and (not current-prefix-arg) discourse-default-url)
             (completing-read "Discourse URL: "
                              (append discourse-saved-sites
                                      (mapcar #'car discourse-site-configs))
                              nil nil "https://"))))
  (let ((discourse-api-auth-method (discourse--site-auth-method url)))
    (let ((site (discourse-api-connect url)))
      (if site
          (progn
            ;; Remember this site
            (unless (member url discourse-saved-sites)
              (push url discourse-saved-sites))
            (discourse-ui-show-categories))
        (user-error "Failed to connect to %s" url)))))

;;;###autoload
(defun discourse ()
  "Open Discourse.
If already connected, show categories for current site.
If `discourse-default-url' is set, connect to it.
Otherwise prompt for a URL."
  (interactive)
  (if discourse--current-site
      (discourse-ui-show-categories)
    (if discourse-default-url
        (discourse-connect discourse-default-url)
      (call-interactively #'discourse-connect))))

;;;###autoload
(defun discourse-switch-site ()
  "Switch to a different Discourse site.
Shows currently connected sites and saved sites."
  (interactive)
  (let* ((connected-urls (let (urls)
                           (maphash (lambda (url _) (push url urls))
                                    discourse--sites)
                           urls))
         (all-urls (delete-dups
                    (append connected-urls
                            discourse-saved-sites
                            (mapcar #'car discourse-site-configs))))
         (annotated (mapcar (lambda (url)
                              (if (gethash url discourse--sites)
                                  (format "%s [connected]" url)
                                url))
                            all-urls))
         (choice (completing-read "Switch to Discourse site: "
                                  annotated nil nil "https://"))
         (url (replace-regexp-in-string " \\[connected\\]$" "" choice)))
    (let ((existing (gethash url discourse--sites)))
      (if existing
          (progn
            (setq discourse--current-site existing)
            (discourse-ui-show-categories))
        (discourse-connect url)))))

;;;###autoload
(defun discourse-disconnect ()
  "Disconnect from the current Discourse site."
  (interactive)
  (if discourse--current-site
      (progn
        (let ((url (discourse-site-url discourse--current-site)))
          (discourse-api-disconnect)
          (message "Disconnected from %s" url)))
    (message "Not connected to any site.")))

;;;###autoload
(defun discourse-browse-topic (url)
  "Open a Discourse topic by its full URL.
Parses the URL to extract the topic ID and displays it."
  (interactive "sDiscourse topic URL: ")
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (path (url-filename parsed))
         (base-url (format "%s://%s" (url-type parsed) host)))
    ;; Ensure we're connected to the right site
    (unless (and discourse--current-site
                 (string= base-url (discourse-site-url discourse--current-site)))
      (discourse-connect base-url))
    ;; Extract topic ID from path like /t/slug/12345 or /t/12345
    (if (string-match "/t/[^/]*/\\([0-9]+\\)" path)
        (discourse-ui-show-topic (string-to-number (match-string 1 path)))
      (if (string-match "/t/\\([0-9]+\\)" path)
          (discourse-ui-show-topic (string-to-number (match-string 1 path)))
        (user-error "Cannot parse topic ID from URL: %s" url)))))

;;; --- Keybinding summary (for documentation) ---

;; Category list:
;;   RET   - Open category (show topics)
;;   g     - Refresh categories
;;   L     - Show latest topics (all categories)
;;   s     - Search
;;   n     - Show notifications
;;   q     - Quit
;;
;; Topic list:
;;   RET   - Open topic (show posts)
;;   g     - Refresh topics
;;   N     - Next page
;;   P     - Previous page
;;   c     - Compose new topic
;;   s     - Search
;;   q     - Quit (back to categories)
;;
;; Topic/Post view:
;;   n     - Next post
;;   p     - Previous post
;;   r     - Reply to topic
;;   R     - Reply to post at point
;;   l     - Like post at point
;;   b     - Open in browser
;;   g     - Refresh
;;   SPC   - Scroll down
;;   DEL   - Scroll up
;;   q     - Quit
;;
;; Compose:
;;   C-c C-c - Send
;;   C-c C-k - Cancel
;;   C-c C-p - Preview

(provide 'discourse)

;;; discourse.el ends here
