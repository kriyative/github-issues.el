;;; github-issues.el --- Functions and modes for managing GitHub projects' issues

;; Copyright (C) 2012 Leandro M. López (inkel) <inkel.ar@gmail.com>

;; Author: Leandro M. López (inkel) <inkel.ar@gmail.com>
;; Created: April 19, 2012
;; Version: 0.0.1
;; Keywords: GitHub Issues
;; Package-Requires: ((emacs "24"))
;; URL: http://inkel.github.com/github-issues.el/

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;;; Installation

;; Make sure to place `github-issues.el` somewhere in the load-path
;; and add the following lines to your `.emacs` file:
;;
;;    (require 'github-issues)
;;

;;; Code:

(require 'gh-issues)
(require 'tabulated-list)
(require 'url)
(require 'font-lock)
(require 'json)
(require 'url-http)
(eval-when-compile (require 'cl)) ; for flet

(defun github-issues-parse-response (buffer)
  "Parses the JSON response from a GitHub API call."
  (let ((json-object-type 'plist))
    (unwind-protect
        (with-current-buffer buffer
          (save-excursion
            (url-http-parse-response)
            (goto-char (point-min))
            (search-forward "\n\n")
            (json-read)))
      (kill-buffer buffer))))

(defvar github-issues-current-user nil
  "Current github user.")
(make-variable-buffer-local 'github-issues-current-user)

(defvar github-issues-current-repo nil
  "Current github repo.")
(make-variable-buffer-local 'github-issues-current-repo)

(defvar github-issues-current-issue nil
  "Current github issue.")
(make-variable-buffer-local 'github-issues-current-issue)

(defvar *github-gh-issues-api* nil)

(defun github-gh-issues-api ()
  (unless *github-gh-issues-api*
    (setq *github-gh-issues-api* (gh-issues-api "github-issues-api"
                                                :sync nil
                                                :cache t
                                                :num-retries 1)))
  *github-gh-issues-api*)

(defun github-api-repository-issues (user repo)
  "Returns a list of gh-issues instances."
  (slot-value (gh-issues-issue-list (github-gh-issues-api) user repo) :data))

(defun github-api-repository-issue (user repo number)
  "Return an issue data in `plist` format."
  (let ((url (format "https://api.github.com/repos/%s/%s/issues/%s" user repo number)))
    (github-issues-parse-response (url-retrieve-synchronously url))))

(defun github-issues-buffer (user repo)
  "Creates or return the buffer for the given user and repository."
  (get-buffer-create (format "*GitHub Issues: %s/%s*" user repo)))

(defun github-issue-buffer (user repo number)
  "Creates or return the buffer for the given user, repository and number."
  (get-buffer-create (format "*GitHub Issue: %s/%s*" user repo)))

(defun github-issue-entry-show (&optional button)
  (when button
    (let* ((issue (button-get button 'issue))
           (user github-issues-current-user)
           (repo github-issues-current-repo)
           (buffer (github-issue-buffer user repo (slot-value issue :number))))
      (github-issue-populate buffer issue)
      (with-current-buffer buffer
        (setq github-issues-current-user user)
        (setq github-issues-current-repo repo)
        (setq github-issues-current-issue issue)))))

(defun format-local-timestamp (timestamp)
  (format-time-string "%D %H:%M" (date-to-time timestamp)))

(defun safe-slot-value (object slot-name)
  (let ((v (slot-value object slot-name)))
    (unless (eq 'unbound v)
      v)))

(defun github-tabulated-issue (issue)
  "Formats an issue data to populate the issue list."
  (flet ((pget (prop)
               (format "%s" (slot-value issue prop))))
    (list (cons (pget :url) (pget :number))
          (vector (list (pget :number)
                        'font-lock-builtin-face 'link
                        'follow-link t
                        'issue issue
                        'action 'github-issue-entry-show)
                  (format-local-timestamp (or (slot-value issue :updated-at)
                                              (slot-value issue :created-at)))
                  (pget :state)
                  (slot-value (slot-value issue :user) :login)
                  (or (safe-slot-value (safe-slot-value issue :assignee) :login) "")
                  (propertize (pget :title) 'font-lock-face 'default)))))

(defun github-issue-sort-by-issue (a b)
  "Compare two issues list entries by issue number."
  (flet ((issue (data) (string-to-number (car (aref (cadr data) 0)))))
    (< (issue a) (issue b))))

(defun github-issue-sort-by-title (a b)
  "Compare two issues list entries by title."
  (flet ((title (data) (car (aref (cadr data) 0))))
    (string< (title a) (title b))))

(defun github-issue-sort-col-as-text (n)
  (lexical-let ((n n))
    (lambda (a b)
      (flet ((text (data) (aref (cadr data) n)))
        (string< (text a) (text b))))))

(setf (symbol-function 'github-issue-sort-by-date)
      (github-issue-sort-col-as-text 1))
(setf (symbol-function 'github-issue-sort-by-state)
      (github-issue-sort-col-as-text 2))
(setf (symbol-function 'github-issue-sort-by-reporter)
      (github-issue-sort-col-as-text 3))
(setf (symbol-function 'github-issue-sort-by-assignee)
      (github-issue-sort-col-as-text 4))
(setf (symbol-function 'github-issue-sort-by-title)
      (github-issue-sort-col-as-text 5))

(defun github-issues-populate (buffer issues-plist)
  "Populates the given buffer with a list of issues. See `github-api-repository-issues`."
  (with-current-buffer buffer
    (github-issues-mode)
    (setq tabulated-list-entries
          (mapcar 'github-tabulated-issue issues-plist))
    (tabulated-list-print nil)
    (github-switch-to-buffer buffer)))

(defun github-issue-colorize-label (label)
  (flet ((pget (label key) (slot-value label key)))
    (let ((color (format "#%s" (or (pget label 'color) "00f"))))
      (propertize (pget label 'name)
                  'font-lock-face (list :background color)))))

(defun github-browse-issue (issue)
  (browse-url (get-text-property issue 'url)))

(defun github-issue-comments (issue)
  (slot-value (gh-issues-comments-list (github-gh-issues-api)
                                       user
                                       repo
                                       issue-id)
              :data))

(defun github-user-button (user)
  (insert-text-button (slot-value user :login)
                      'font-lock-builtin-face 'link
                      'follow-link t
                      'url (slot-value user :html-url)
                      'action 'github-browse-issue))

(defun github-issue-comments-populate (issue)
  (let* ((user github-issues-current-user)
         (repo github-issues-current-repo)
         (issue-id (slot-value issue :number))
         (comments (github-issue-comments issue)))
    (dolist (comment comments)
      (insert "\n---- On "
              (format-local-timestamp (slot-value comment :updated_at))
              " ")
      (github-user-button (slot-value comment :user))
      (insert " wrote:\n\n")
      (insert (slot-value comment :body))
      (insert "\n"))))

(defun github-issue-populate (buffer issue)
  "Populates the given buffer with issue data. See `github-api-repository-issue`."
  (flet ((pget (key) (safe-slot-value issue key)))
    (with-current-buffer buffer
      (toggle-read-only -1)
      (erase-buffer)
      (insert (format "%8s: " (concat "#" (number-to-string (pget :number)))))
      (insert-text-button (pget :title)
                          'font-lock-builtin-face 'link
                          'follow-link t
                          'url (pget :html-url)
                          'action 'github-browse-issue)
      (insert "\n Created: " (format-local-timestamp (pget :created-at)))
      (insert "\n Updated: " (format-local-timestamp (pget :updated-at)))
      (insert "\nReporter: ") (github-user-button (pget :user))
      (let ((assignee (pget :assignee)))
        (when (and assignee (safe-slot-value assignee :login))
          (insert "\nAssignee: ") (github-user-button assignee)))
      (when (> (length (pget :labels)) 0)
        (insert "\n  Labels: ")
        (dolist (label (pget :labels))
          (insert "[" (github-issue-colorize-label label) "]")))
      (let ((beg (point)))
        (insert "\n\n" (pget :body) "\n\n")
        (github-issue-comments-populate issue)
        (replace-string "" "" nil beg (point)))
      (github-issue-mode)
      (visual-line-mode)
      (github-switch-to-buffer buffer))))

;;;###autoload
(defun github-issues (user repo)
  "Display a list of issues list for a GitHub repository."
  (interactive
   (list (read-string "GitHub username: " nil 'github-username-history t)
         (read-string "Repository: " nil 'github-repository-history t)))
  (when (and user repo)
    (with-current-buffer (github-issues-buffer user repo)
      (if github-issues-current-user
          (github-switch-to-buffer (current-buffer))
        (github-issues-refresh user repo)))))

(defun github-switch-to-buffer (buffer)
  (let ((window (get-buffer-window buffer)))
    (if window
        (select-window window)
      (switch-to-buffer-other-window buffer))))

(defun github-issues-refresh (&optional user repo)
  "Refresh GitHub issues list."
  (interactive)
  (let ((user (or user github-issues-current-user))
        (repo (or repo github-issues-current-repo)))
    (github-issues-populate (github-issues-buffer user repo)
                            (github-api-repository-issues user repo))
    (setq github-issues-current-user user)
    (setq github-issues-current-repo repo)))

(defun github-issue-refresh (&optional user repo number)
  "Refresh GitHub issue data."
  (interactive)
  (let ((user (or user github-issues-current-user))
        (repo (or repo github-issues-current-repo))
        (number (slot-value github-issues-current-issue :number)))
    (github-issue-populate (github-issue-buffer user repo number)
                           (github-api-repository-issue user repo number))
    (setq github-issues-current-user user)
    (setq github-issues-current-repo repo)
    (setq github-issues-current-issue github-issues-current-issue)))

(defun github-issue-browse ()
  "Open the current issue in a web browser."
  (interactive)
  (if github-issues-current-issue
      (browse-url (slot-value github-issues-current-issue :html-url))
    (message "No current issue selected")))

(defun github-issue-browse-author ()
  "Open the current issue's author profile in a web browser."
  (interactive)
  (if github-issues-current-issue
      (browse-url (format "https://github.com/%s"
                          (slot-value (slot-value github-issues-current-issue :user) :login)))
    (message "No current issue selected")))

(defvar github-issues-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'github-issues-refresh)
    map)
  "Keymap for GitHub Issues major mode.")

(define-derived-mode github-issues-mode tabulated-list-mode "GitHub Issues"
  "Major mode for browsing a list of issues in a GitHub project.

\\{github-issues-mode-map}"
  (setq tabulated-list-format [("Issue" 5 github-issue-sort-by-issue)
                               ("Date"  15 github-issue-sort-by-date)
                               ("State" 5 github-issue-sort-by-state)
                               ("Reporter" 10 github-issue-sort-by-reporter)
                               ("Assignee" 10 github-issue-sort-by-assignee)
                               ("Title" 60 github-issue-sort-by-title)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Issue" nil))
  (tabulated-list-init-header))

(defvar github-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'github-issue-refresh)
    (define-key map "b" 'github-issue-browse)
    (define-key map "a" 'github-issue-browse-author)
    map)
  "Keymap for GitHub Issue major mode.")

(define-derived-mode github-issue-mode special-mode "GitHub Issue"
  "Major mode for display a GitHub issue data.

\\{github-issue-mode-map}")

(provide 'github-issues)

;;; github-issues.el ends here
