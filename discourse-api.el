;;; discourse-api.el --- Discourse REST API client  -*- lexical-binding: t; coding: utf-8 -*-

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

;; HTTP client and REST API wrapper for Discourse forums.
;; Supports two authentication methods:
;;   1. API key auth (Api-Key + Api-Username headers)
;;   2. Session-based auth (CSRF token + cookie login)
;;
;; Credentials are read from .authinfo.gpg via auth-source.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(require 'auth-source)
(require 'mm-url)

;;; --- Connection structures ---

(cl-defstruct (discourse-site (:constructor discourse-site--create))
  "Represents a connection to a Discourse forum."
  url
  username
  auth-method ; 'api-key or 'session
  api-key
  csrf-token
  session-cookie)

(defvar discourse--sites (make-hash-table :test 'equal)
  "Hash table mapping site URL strings to `discourse-site' structs.")

(defvar discourse--current-site nil
  "The currently active `discourse-site'.")

;;; --- Customization ---

(defgroup discourse-api nil
  "Discourse API settings."
  :group 'discourse
  :prefix "discourse-api-")

(defcustom discourse-api-auth-method 'auto
  "Authentication method for Discourse sites.
`api-key' uses Api-Key + Api-Username headers.
`session' uses CSRF token + session cookie login.
`auto' tries api-key first, falls back to session."
  :type '(choice (const :tag "API Key" api-key)
                 (const :tag "Session (username/password)" session)
                 (const :tag "Auto-detect" auto))
  :group 'discourse-api)

(defcustom discourse-api-auth-source-file nil
  "Specific auth-source file for Discourse credentials.
When nil, uses the default `auth-sources'."
  :type '(choice (const :tag "Default auth-sources" nil)
                 (file :tag "Specific file"))
  :group 'discourse-api)

;;; --- Auth-source integration (ported from nndiscourse) ---

(defun discourse-api--get-auth-info (host)
  "Get authentication info for HOST from auth-source.
Returns a plist (:user USER :secret SECRET) or nil."
  (let ((auth-sources (if discourse-api-auth-source-file
                          (list discourse-api-auth-source-file)
                        auth-sources)))
    (condition-case err
        (let* ((auth-source-creation-prompts
                '((user . "Discourse user at %h: ")
                  (secret . "Discourse API key or password for %u@%h: ")))
               (auth-info (car (auth-source-search :host host
                                                   :require '(:user :secret)
                                                   :create t
                                                   :max 1))))
          (when auth-info
            (let ((secret (plist-get auth-info :secret)))
              (list :user (plist-get auth-info :user)
                    :secret (if (functionp secret)
                                (funcall secret)
                              secret)))))
      (error
       (message "discourse-api: auth-source error for %s: %s"
                host (error-message-string err))
       (let ((user (read-string (format "Discourse user at %s: " host)))
             (secret (read-passwd (format "Discourse API key or password for %s: " host))))
         (list :user user :secret secret))))))

;;; --- HTTP request infrastructure ---

(defun discourse-api--parse-json-response ()
  "Parse JSON response from current URL buffer.
Returns parsed JSON or nil on failure."
  ;; Force buffer to unibyte so raw bytes are preserved for proper decoding
  (set-buffer-multibyte nil)
  (goto-char (point-min))
  (when (re-search-forward "^\r?\n" nil t)
    (let* ((start (point))
           (json-string (decode-coding-string
                         (buffer-substring-no-properties start (point-max))
                         'utf-8)))
      (condition-case err
          (let ((json-object-type 'alist)
                (json-array-type 'vector)
                (json-key-type 'symbol))
            (json-read-from-string json-string))
        (json-readtable-error
         (message "discourse-api: JSON parse error: %s"
                  (error-message-string err))
         nil)))))

(defun discourse-api--extract-cookies (buffer)
  "Extract Set-Cookie headers from URL response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (cookies)
      (while (re-search-forward "^Set-Cookie: \\([^;]+\\)" nil t)
        (push (match-string 1) cookies))
      (when cookies
        (mapconcat #'identity (nreverse cookies) "; ")))))

(defun discourse-api--build-headers (site &optional extra-headers)
  "Build HTTP headers for SITE request with optional EXTRA-HEADERS."
  (let ((headers (append
                  (list (cons "Accept" "application/json")
                        (cons "User-Agent" "discourse.el/0.1"))
                  extra-headers)))
    (when site
      (cl-case (discourse-site-auth-method site)
        (api-key
         (when (discourse-site-api-key site)
           (push (cons "Api-Key" (discourse-site-api-key site)) headers))
         (when (discourse-site-username site)
           (push (cons "Api-Username" (discourse-site-username site)) headers)))
        (session
         (when (discourse-site-csrf-token site)
           (push (cons "X-CSRF-Token" (discourse-site-csrf-token site)) headers))
         (when (discourse-site-session-cookie site)
           (push (cons "Cookie" (discourse-site-session-cookie site)) headers)))))
    headers))

(defun discourse-api--request (method url-path &optional data site)
  "Make an HTTP request to the Discourse API.
METHOD is \"GET\", \"POST\", \"PUT\", or \"DELETE\".
URL-PATH is the path (e.g. \"/categories.json\").
DATA is an alist to JSON-encode as the request body.
SITE is a `discourse-site' struct (defaults to `discourse--current-site').
Returns parsed JSON response or nil on failure."
  (let* ((site (or site discourse--current-site))
         (base-url (and site (discourse-site-url site)))
         (full-url (concat base-url url-path))
         (url-request-method method)
         (url-request-extra-headers
          (discourse-api--build-headers
           site
           (when data
             (list (cons "Content-Type" "application/json")))))
         (url-request-data
          (when data
            (encode-coding-string (json-encode data) 'utf-8)))
         (url-mime-accept-string "application/json"))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously full-url t nil 30)
          (let* ((status-code (url-http-symbol-value-in-buffer
                               'url-http-response-status (current-buffer)))
                 (result (discourse-api--parse-json-response)))
            (when (and (numberp status-code) (>= status-code 400))
              (message "discourse-api: HTTP %s %s returned %d"
                       method url-path status-code))
            (kill-buffer (current-buffer))
            (if (and (numberp status-code) (>= status-code 400))
                (list (cons 'error t)
                      (cons 'status status-code)
                      (cons 'body result))
              result)))
      (error
       (message "discourse-api: request error %s %s: %s"
                method full-url (error-message-string err))
       nil))))

(defun discourse-api--get (path &optional site)
  "GET request to PATH on SITE."
  (discourse-api--request "GET" path nil site))

(defun discourse-api--post (path data &optional site)
  "POST request to PATH with DATA on SITE."
  (discourse-api--request "POST" path data site))

(defun discourse-api--put (path data &optional site)
  "PUT request to PATH with DATA on SITE."
  (discourse-api--request "PUT" path data site))

(defun discourse-api--delete (path &optional site)
  "DELETE request to PATH on SITE."
  (discourse-api--request "DELETE" path nil site))

;;; --- CSRF / Session auth (ported from nndiscourse) ---

(defun discourse-api--get-csrf-token (site)
  "Fetch CSRF token for SITE.  Returns the token string or nil."
  (let* ((url (concat (discourse-site-url site) "/session/csrf"))
         (url-request-method "GET")
         (url-request-extra-headers
          (list (cons "Accept" "application/json")
                (cons "User-Agent" "discourse.el/0.1")))
         (url-mime-accept-string "application/json"))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t nil 15)
          (let ((data (discourse-api--parse-json-response)))
            (prog1 (alist-get 'csrf data)
              (kill-buffer (current-buffer)))))
      (error
       (message "discourse-api: CSRF token request failed: %s"
                (error-message-string err))
       nil))))

(defun discourse-api--session-login (site)
  "Log in to SITE using username/password, setting CSRF token and session cookie.
Returns non-nil on success."
  (let* ((host (url-host (url-generic-parse-url (discourse-site-url site))))
         (auth (discourse-api--get-auth-info host)))
    (if (null auth)
        (progn (message "discourse-api: no credentials found for host %s" host) nil)
      (message "discourse-api: found credentials for %s (user: %s)"
               host (plist-get auth :user))
      (let ((csrf (discourse-api--get-csrf-token site)))
        (if (null csrf)
            (progn (message "discourse-api: failed to get CSRF token from %s"
                           (discourse-site-url site))
                   nil)
          (message "discourse-api: got CSRF token: %s" (substring csrf 0 (min 10 (length csrf))))
          (setf (discourse-site-csrf-token site) csrf)
          (let* ((login-url (concat (discourse-site-url site) "/session"))
                 (url-request-method "POST")
                 (url-request-extra-headers
                  (list (cons "Accept" "application/json")
                        (cons "Content-Type" "application/json")
                        (cons "X-CSRF-Token" csrf)
                        (cons "X-Requested-With" "XMLHttpRequest")
                        (cons "User-Agent" "discourse.el/0.1")))
                 (url-request-data
                  (encode-coding-string
                   (json-encode `((login . ,(plist-get auth :user))
                                 (password . ,(plist-get auth :secret))))
                   'utf-8))
                 (url-mime-accept-string "application/json"))
            (message "discourse-api: POSTing login to %s" login-url)
            (condition-case err
                (with-current-buffer (url-retrieve-synchronously login-url t nil 15)
                  (let* ((status (url-http-symbol-value-in-buffer
                                  'url-http-response-status (current-buffer)))
                         (cookies (discourse-api--extract-cookies (current-buffer)))
                         (response-data (discourse-api--parse-json-response)))
                    (message "discourse-api: login response status: %s" status)
                    (message "discourse-api: login response body: %S" response-data)
                    (message "discourse-api: login cookies: %S" cookies)
                    (kill-buffer (current-buffer))
                    (if cookies
                        (progn
                          (setf (discourse-site-session-cookie site) cookies)
                          (setf (discourse-site-username site) (plist-get auth :user))
                          (setf (discourse-site-auth-method site) 'session)
                          t)
                      (message "discourse-api: login succeeded but no cookies returned")
                      nil)))
              (error
               (message "discourse-api: session login error: %s"
                        (error-message-string err))
               nil))))))))

;;; --- Site connection ---

(defun discourse-api-connect (url)
  "Connect to a Discourse forum at URL.
Looks up credentials from auth-source and authenticates.
Returns a `discourse-site' struct on success."
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (base-url (concat (url-type parsed) "://" host))
         (auth (discourse-api--get-auth-info host))
         (site (discourse-site--create
                :url base-url
                :username (plist-get auth :user))))
    (if (or (eq discourse-api-auth-method 'api-key)
            (eq discourse-api-auth-method 'auto))
        ;; Try API key auth first
        (progn
          (setf (discourse-site-auth-method site) 'api-key)
          (setf (discourse-site-api-key site) (plist-get auth :secret))
          ;; Test the connection
          (let ((test (discourse-api--get "/categories.json" site)))
            (if (and test (not (alist-get 'error test)))
                (progn
                  (message "discourse-api: connected to %s via API key" base-url)
                  (puthash base-url site discourse--sites)
                  (setq discourse--current-site site)
                  site)
              ;; API key failed; try session if auto
              (if (eq discourse-api-auth-method 'auto)
                  (progn
                    (message "discourse-api: API key auth failed, trying session login...")
                    (setf (discourse-site-api-key site) nil)
                    (if (discourse-api--session-login site)
                        (progn
                          (message "discourse-api: connected to %s via session" base-url)
                          (puthash base-url site discourse--sites)
                          (setq discourse--current-site site)
                          site)
                      (message "discourse-api: all auth methods failed for %s" base-url)
                      nil))
                (message "discourse-api: API key auth failed for %s" base-url)
                nil))))
      ;; Session auth only
      (if (discourse-api--session-login site)
          (progn
            (message "discourse-api: connected to %s via session" base-url)
            (puthash base-url site discourse--sites)
            (setq discourse--current-site site)
            site)
        (message "discourse-api: session auth failed for %s" base-url)
        nil))))

(defun discourse-api-disconnect (&optional site)
  "Disconnect from SITE (defaults to current site)."
  (let ((site (or site discourse--current-site)))
    (when site
      (remhash (discourse-site-url site) discourse--sites)
      (when (eq site discourse--current-site)
        (setq discourse--current-site nil)))))

;;; --- API endpoint wrappers ---

;; Categories

(defun discourse-api-get-categories (&optional site)
  "Fetch all categories from SITE.
Returns a vector of category alists."
  (when-let* ((data (discourse-api--get "/categories.json" site))
              (cats (alist-get 'category_list data)))
    (alist-get 'categories cats)))

;; Topics

(defun discourse-api-get-latest-topics (&optional page site)
  "Fetch latest topics, optionally at PAGE, from SITE.
Returns an alist with `topic_list' containing `topics' vector."
  (let ((path (if (and page (> page 0))
                  (format "/latest.json?page=%d" page)
                "/latest.json")))
    (discourse-api--get path site)))

(defun discourse-api-get-category-topics (slug category-id &optional page site)
  "Fetch topics for category SLUG with CATEGORY-ID, optionally at PAGE, from SITE."
  (let ((path (if (and page (> page 0))
                  (format "/c/%s/%d/l/latest.json?page=%d" slug category-id page)
                (format "/c/%s/%d/l/latest.json" slug category-id))))
    (discourse-api--get path site)))

(defun discourse-api-get-topic (topic-id &optional site)
  "Fetch a single topic by TOPIC-ID from SITE.
Returns the full topic alist including the post stream."
  (discourse-api--get (format "/t/%d.json" topic-id) site))

(defun discourse-api-get-topic-posts (topic-id post-ids &optional site)
  "Fetch specific posts by POST-IDS (list of ints) from TOPIC-ID on SITE."
  (let ((ids-param (mapconcat #'number-to-string post-ids ",")))
    (discourse-api--get
     (format "/t/%d/posts.json?post_ids[]=%s" topic-id ids-param)
     site)))

;; Posts

(defun discourse-api-get-post (post-id &optional site)
  "Fetch a single post by POST-ID from SITE."
  (discourse-api--get (format "/posts/%d.json" post-id) site))

(defun discourse-api-create-topic (title raw category-id &optional site)
  "Create a new topic with TITLE, RAW markdown body, in CATEGORY-ID on SITE."
  (discourse-api--post "/posts.json"
                       `((title . ,title)
                         (raw . ,raw)
                         (category . ,category-id))
                       site))

(defun discourse-api-create-reply (topic-id raw &optional reply-to-post-number site)
  "Create a reply in TOPIC-ID with RAW body on SITE.
If REPLY-TO-POST-NUMBER is provided, this is a reply to that specific post."
  (let ((data `((topic_id . ,topic-id)
                (raw . ,raw))))
    (when reply-to-post-number
      (push (cons 'reply_to_post_number reply-to-post-number) data))
    (discourse-api--post "/posts.json" data site)))

(defun discourse-api-update-post (post-id raw &optional site)
  "Update POST-ID with new RAW body on SITE."
  (discourse-api--put (format "/posts/%d.json" post-id)
                      `((post ((raw . ,raw))))
                      site))

;; Actions (like, bookmark, etc.)

(defun discourse-api-like-post (post-id &optional site)
  "Like POST-ID on SITE.  Post action type 2 = like."
  (discourse-api--post "/post_actions.json"
                       `((id . ,post-id)
                         (post_action_type_id . 2))
                       site))

(defun discourse-api-unlike-post (post-id &optional site)
  "Remove like from POST-ID on SITE."
  (discourse-api--delete
   (format "/post_actions/%d.json?post_action_type_id=2" post-id)
   site))

(defun discourse-api-bookmark-topic (topic-id &optional site)
  "Bookmark TOPIC-ID on SITE."
  (discourse-api--put (format "/t/%d/bookmark.json" topic-id) nil site))

;; Search

(defun discourse-api-search (query &optional site)
  "Search for QUERY on SITE."
  (discourse-api--get
   (format "/search.json?q=%s" (url-hexify-string query))
   site))

;; User

(defun discourse-api-get-user (username &optional site)
  "Fetch user profile for USERNAME from SITE."
  (discourse-api--get (format "/u/%s.json" username) site))

(defun discourse-api-get-notifications (&optional site)
  "Fetch notifications for the authenticated user from SITE."
  (discourse-api--get "/notifications.json" site))

(defun discourse-api-get-unread-topics (&optional site)
  "Fetch unread topics for the authenticated user from SITE."
  (discourse-api--get "/unread.json" site))

(defun discourse-api-get-new-topics (&optional site)
  "Fetch new topics for the authenticated user from SITE."
  (discourse-api--get "/new.json" site))

(defun discourse-api-get-user-topics (username &optional site)
  "Fetch topics created by USERNAME from SITE."
  (discourse-api--get (format "/topics/created-by/%s.json" username) site))

(defun discourse-api-get-private-messages (username &optional site)
  "Fetch private messages for USERNAME from SITE."
  (discourse-api--get (format "/topics/private-messages/%s.json" username) site))

(defun discourse-api-get-categories-with-counts (&optional site)
  "Fetch categories including subcategories with unread/new counts from SITE.
Returns a flat list of all categories including subcategories."
  (when-let* ((data (discourse-api--get
                     "/categories.json?include_subcategories=true"
                     site))
              (cats (alist-get 'category_list data))
              (top-cats (alist-get 'categories cats)))
    ;; Flatten: collect top-level categories and their subcategories
    (let ((all-cats nil))
      (seq-doseq (cat (if (vectorp top-cats) top-cats (vconcat top-cats)))
        (push cat all-cats)
        ;; Check for subcategory_list (vector of sub-category alists)
        (let ((subcats (alist-get 'subcategory_list cat)))
          (when subcats
            (seq-doseq (sub (if (vectorp subcats) subcats (vconcat subcats)))
              (push sub all-cats))))
)
      (nreverse all-cats))))

(provide 'discourse-api)

;;; discourse-api.el ends here
