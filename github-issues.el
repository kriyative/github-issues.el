;;; github-issues.el --- Emacs utility functions and modes for managing GitHub projects' issues

;; Copyright (C) 2012 Leandro M. López (inkel) <inkel.ar@gmail.com>

;; Author: Leandro M. López (inkel) <inkel.ar@gmail.com>
;; Created: April 19, 2012
;; Version: 0.0.1
;; Keywords: GitHub Issues
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

;;; Installation

;; Make sure to place `github-issues.el` somewhere in the load-path and add the following lines to your `.emacs` file:
;;
;;    (require 'github-issues)
;;

(require 'tabulated-list)
(require 'url)
(require 'font-lock)

(defun github-parse-response (buffer)
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

(defun github-api-repository-issues (user repo)
  "Returns a list of issues in `plist` format."
  (let ((url (format "https://api.github.com/repos/%s/%s/issues" user repo)))
    (github-parse-response (url-retrieve-synchronously url))))

(defun github-issues-buffer (user repo)
  "Creates or return the buffer for the given user and repository."
  (get-buffer-create (format "*GitHub Issues: %s/%s*" user repo)))

;;; TODO: I'm not convinced at all of this function
(defun github-issue-entry-show (&optional button)
  (let ((user (car github-username-history))
        (repo (car github-repository-history))
        (number (cdr (tabulated-list-get-id))))
    (message "Not implemented")))

(defun github-tabulated-issue (issue)
  "Formats an issue data to populate the issue list."
  (flet ((pget (prop)
               (format "%s" (plist-get issue prop))))
    (list (cons (pget :id) (pget :number))
          (vector (list (pget :number)
                        'font-lock-builtin-face 'link
                        'follow-link t
                        'url (plist-get (pget :user) :url)
                        'action 'github-issue-entry-show)
                  (propertize (pget :title) 'font-lock-face 'default)))))

(defun github-issue-sort-by-issue (a b)
  "Compare two issues list entries by issue number."
  (flet ((issue (data) (string-to-number (car (aref (cadr data) 0)))))
    (< (issue a) (issue b))))

(defun github-issue-sort-by-title (a b)
  "Compare two issues list entries by title."
  (flet ((title (data) (car (aref (cadr data) 0))))
    (string< (title a) (title b))))

(defun github-issues-populate (buffer issues-plist)
  "Populates the given buffer with a list of issues. See `github-api-repository-issues`."
  (with-current-buffer buffer
    (github-issues-mode)
    (setq tabulated-list-entries
          (mapcar 'github-tabulated-issue issues-plist))
    (tabulated-list-print nil)
    (switch-to-buffer-other-window buffer)))

(defun github-issues (user repo)
  "Display a list of issues list for a GitHub repository."
  (interactive
   (list (read-string "GitHub username: " nil 'github-username-history t)
         (read-string "Repository: " nil 'github-repository-history t)))
  (if (and user repo)
      (with-current-buffer (github-issues-buffer user repo)
        (if (boundp 'github-current-user)
            (switch-to-buffer-other-window (current-buffer))
          (github-issues-refresh user repo)
          (setq github-current-user user)
          (setq github-current-repo repo)))))

(defun github-issues-refresh (&optional user repo)
  "Refresh."
  (interactive)
  (if (not user)
    (setq user github-current-user))
  (if (not repo)
    (setq repo github-current-repo))
  (github-issues-populate (github-issues-buffer user repo)
                          (github-api-repository-issues user repo)))

(defvar github-issues-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-cr" 'github-issues-refresh)
    map)
  "Keymap for GitHub Issues major mode.")

(define-derived-mode github-issues-mode tabulated-list-mode "GitHub Issues"
  "Major mode for browsing a list of issues in a GitHub project.

\\{github-issues-mode-map}"
  (setq tabulated-list-format [("Issue" 5 github-issue-sort-by-issue)
                               ("Title" 60 github-issue-sort-by-title)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Issue" nil))
  (make-local-variable 'github-current-user)
  (make-local-variable 'github-current-repo)
  (tabulated-list-init-header))

(define-derived-mode github-issue-mode font-lock-mode "GitHub Issue"
  "Major mode for display a GitHub issue data."
  (toggle-read-only t))

(provide 'github-issues)
