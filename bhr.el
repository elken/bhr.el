;;; bhr.el --- Convenience package for BambooHR -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: January 25, 2023
;; Modified: January 25, 2023
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/elken/bamboo.el
;; Package-Requires: ((emacs "28.1") (ts "0.2.2") (a "1.0.0") (tablist "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Convenience package for BambooHR
;;
;;; Code:

(require 'url)
(require 'ts)
(require 'org)
(require 'a)
(require 'tablist)
(eval-when-compile (require 'cl-lib))

;;;###autoload
(defgroup bhr nil
  "Convenience package for BambooHR."
  :group 'external)

(defcustom bhr-org-name
  "bamboo"
  "The name of the organisation managing Bamboo."
  :type 'string
  :group 'bhr)

(defcustom bhr-include-weekends nil
  "Whether or not to include weekends on views that display day data."
  :type 'boolean
  :group 'bhr)

(defcustom bhr-default-hours 8
  "The default amount of hours to populate for an entry.
Also used to indicate a halfway point to display during date selection."
  :type 'integer
  :group 'bhr)

(defvar bhr-timesheet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'tablist-edit-column)
    map)
  "Keymap for `bhr-timesheet-mode'.")

(defvar bhr--csrf-token nil)
(defvar bhr--session-user nil)
(defvar bhr--session-left nil)
(defvar bhr--time-tracking nil)
(defvar bhr--task-history nil)

(defun bhr--site-url ()
  "Return the main site URL for the current bamboo instance."
  (concat bhr-org-name ".bamboohr.com"))

(defun bhr--url (route)
  "Return the URL for ROUTE on the current bamboo instance."
  (concat "https://" (bhr--site-url) "/" route))

(defun bhr--find-sexp (regex &optional buffer)
  "Find REGEX in the BUFFER or current buffer and return the next sexp.
This includes things like strings and {} pairs."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (let (beg)
        (search-forward regex)
        (setq beg (point))
        (forward-sexp)
        (buffer-substring beg (point))))))

(defun bhr--find-json (regex &optional buffer)
  "Find REGEX in the BUFFER or current buffer and return the next sexp as JSON.
This includes things like strings and {} pairs."
  (json-parse-string (bhr--find-sexp regex buffer) :object-type 'alist :null-object "" :array-type 'list))

(defun bhr--cookies ()
  "Return all the necessary cookies for bamboo."
  (mapconcat
   (lambda (url-cookie)
     (concat (url-hexify-string (url-cookie-name url-cookie))
             "="
             (url-hexify-string (url-cookie-value url-cookie))))
   (cl-merge 'list
             (cdr (assoc (bhr--site-url) url-cookie-secure-storage))
             (cdr (assoc ".bamboohr.com" url-cookie-secure-storage))
             (lambda (lhs rhs)
               (string= (url-cookie-name lhs) (url-cookie-name rhs))))
   ";"))

(defun bhr--alist->url (alist)
  "Convert an alist to a suitable format for URLs."
  (mapconcat
   (lambda (cell)
     (concat (url-hexify-string (car cell))
             "="
             (url-hexify-string (cdr cell))))
   alist
   "&"))

(defun bhr--get-date-range ()
  "Given two user-selected dates, return an inclusive range of those dates."
  (let* ((start-date (org-read-date nil nil nil "Select start date"))
         (end-date (ts-parse (org-read-date nil nil nil "Select end date" (date-to-time start-date))))
         (start-date (ts-parse start-date))
         (duration (ts-human-duration
                    (ts-difference end-date start-date)))
         (delta (+ 1 (plist-get duration :days) (* 365 (plist-get duration :years))))
         (days (cl-loop
                repeat delta
                for start = start-date then (ts-inc 'day 1 start)
                collect
                (ts-format "%Y-%m-%d" start))))
    (if bhr-include-weekends
        days
      (cl-loop
       for day in days
       when (not (member (ts-day-name (ts-parse day)) '("Saturday" "Sunday")))
       collect day))))

(defun bhr--get-task-by-id (task-id)
  "Given a TASK-ID, return a cons cell of (task . project)."
  (cl-loop
   for project in (map-nested-elt bhr--time-tracking '(projectsWithTasks byId))
   for tasks = (cdadr (assoc 'tasks project))
   when (not (null tasks))
   return
   (cl-loop
    for task in tasks
    when (= task-id (alist-get 'id task))
    return task)))

(defun bhr--list-tasks ()
  "Get a listing of all the tasks and their associated project ID."
  (cl-loop
   for project in (map-nested-elt bhr--time-tracking '(projectsWithTasks byId))
   for tasks = (cdadr (assoc 'tasks project))
   append
   (if (null tasks)
       `((,(alist-get 'name project) . ((id . ,(alist-get 'id project))
                                        (name . ,(alist-get 'name project)))))
     (cl-loop
      for task in tasks
      collect
      `(,(alist-get 'name task) . ((id . ,(alist-get 'id task))
                                   (name . ,(alist-get 'name task))
                                   (project . ((id . ,(alist-get 'id project))
                                               (name . ,(alist-get 'name project))))))))))

(cl-defun bhr--request (endpoint &key verb data headers noninteractive json noauth sync callback)
  "Helper function to wrap around querying an ENDPOINT.
Specify an optional callback to run with the buffer as the result of the
operation.

VERB is the HTTP method for the request
DATA is the request body
HEADERS are any headers to be merged with the existing values
NONINTERACTIVE will not prompt the user for anything (such as incorrect
credentials)
JSON is a flag to specify you want to recieve json from the API
NOAUTH won't send the CSRF token or cookies, since bamboo doesn't handle expired
credentials right
SYNC has the request run synchronously"
  (let* ((url-request-method (or verb "GET"))
         (url-cookie-trusted-urls '(".*"))
         (url-request-noninteractive noninteractive)
         (url-request-extra-headers
          (a-merge `(("Referer" . ,endpoint))
                   (unless noauth
                     `(("X-CSRF-TOKEN" . ,bhr--csrf-token)
                       ("Cookie" . ,(bhr--cookies))))
                   (when (member verb '("PUT" "POST" "DELETE"))
                     '(("Content-Type" . "application/json;charset=UTF-8")))
                   (when json
                     '(("Accept" . "application/json, text/plain, */*")))
                   headers))
         (url-request-data data))
    (if sync
        (with-current-buffer (url-retrieve-synchronously endpoint)
          (funcall (or callback #'identity) url-http-response-status))
      (url-retrieve
       endpoint
       (or callback #'identity)))))

(defun bhr-trusted-browser-expired-p ()
  "Predicate to check if the current trusted_browser cookie has expired."
  (if-let ((cookie
            (thread-last
              url-cookie-secure-storage
              (assoc (bhr--site-url))
              cdr
              (cl-find-if (lambda (cell)
                            (string= "trusted_browser" (url-cookie-name cell)))))))
      (url-cookie-expired-p cookie)
    t))

(defun bhr-login ()
  "Send a login request to get token information.
Also checks for the trusted browser cookie, and sets the necessary user
information from the home page."
  (if-let ((auth (auth-source-search :host (bhr--site-url) :max 1)))
      (let* ((headers '(("Content-Type" . "application/x-www-form-urlencoded")))
             (secret (car auth))
             (data
              (bhr--alist->url
               `(("tz" . "Europe/London")
                 ("r" . "/home/")
                 ("username" . ,(plist-get secret :user))
                 ("password" . ,(funcall (plist-get secret :secret)))
                 ("login" . "Log-in")
                 ("CSRFToken" . "")))))
        ;; Attempt to log in
        (bhr--request
         (bhr--url "login.php")
         :callback
         (lambda (_)
           (setq bhr--csrf-token (bhr--find-sexp "CSRF_TOKEN = \""))

           ;; Check trusted browser status
           (when (bhr-trusted-browser-expired-p)
             (bhr--request (bhr--url "auth/trusted_browser") :verb "POST"))

           ;; Scrape info from the home page
           (bhr--request
            (bhr--url "home")
            (lambda (_)
              (setq bhr--session-user (bhr--find-json "SESSION_USER=")
                    bhr--time-tracking (bhr--find-json "window.time_tracking = ")))
            :sync t))
         :verb "POST"
         :data data
         :headers headers
         :sync t
         :noauth t))
    (error "No auth source setup for %s. Double check your setup" (bhr--site-url))))

(defmacro bhr-ensure-session (&rest body)
  "Run BODY while ensuring the current session is valid.
Required for any function that requests data."
  `(progn
     (when (or (null bhr--session-left)
               (ts>= (ts-now) bhr--session-left))
       (bhr-login) ;; This is needed until my patch is merged
       (bhr--request
        (bhr--url "auth/check_session?isOnboarding=false")
        :callback
        (lambda (status)
          (if (= 401 status)
              (bhr-login)
            (let ((json-array-type 'list))
              (goto-char url-http-end-of-headers)
              (let-alist (json-read)
                (setq bhr--session-left (ts-adjust 'minute .SessionMinutesLeft (ts-now))
                      bhr--csrf-token .CSRFToken)))))
        :noninteractive t
        :sync t))
     ,@body))

(defun bhr-get-timesheet-data ()
  "Get the current timesheet data."
  (bhr-ensure-session
   (let ((url-request-method "GET")
         (url-cookie-trusted-urls '(".*"))
         (buffer (url-retrieve-synchronously
                  (bhr--url (concat "timesheet/" (number-to-string (alist-get 'id bhr--time-tracking))))))
         (json-array-type 'list))
     (with-current-buffer buffer
       (goto-char url-http-end-of-headers)
       (json-read)))))

(defun bhr-delete (&rest entries)
  "Delete a list of ENTRIES by ID."
  (bhr-ensure-session
   (bhr--request
    (bhr--url "timesheet/hour/entries")
    :verb "DELETE"
    :data (json-encode `(:entries ,entries))
    :json t
    :noninteractive t)))

(defun bhr--timestamp-affix-function (cands)
  (cl-loop
   for cand in cands
   collect
   (let* ((day (assoc (intern cand) minibuffer-completion-table))
          (hours (alist-get 'hours day))
          (hours (if (numberp hours) hours 0))
          (date (ts-format "%A, %d %B %Y" (ts-parse cand))))
     (list
      date
      ""
      (concat
       (string-pad " " (- 30 (length date)))
       (propertize
        (number-to-string hours)
        'face
        (cond
         ((= hours bhr-default-hours) 'success)
         ((= hours 0) 'error)
         (t 'warning))))))))

(defun bhr--operations-function (op &rest args)
  (pcase op
    ('supported-operations '(delete))
    ('delete (apply #'bhr-delete (cl-first args)))))

(define-derived-mode bhr-timesheet-mode tablist-mode "bhr-timesheet-mode"
  "Major mode for viewing BambooHR timesheets."
  (let* ((data (thread-first (bhr-get-timesheet-data)
                             (map-nested-elt '(timesheet dailyDetails))))
         (days (thread-last data
                            (cl-remove-if-not (lambda (day)
                                                (or bhr-include-weekends
                                                    (not (member (ts-day-name (ts-parse (symbol-name (car day)))) '("Saturday" "Sunday"))))))
                            (mapcar #'identity)))
         (completion-extra-properties
          '(:affixation-function bhr--timestamp-affix-function))
         (date (intern (completing-read "Select day to view timesheet: " days nil t nil)))
         (day (cdr (assoc date data))))

    (setq tablist-operations-function #'bhr--operations-function
          tabulated-list-format
          (vector
           (list "Hours" 5 t)
           (list "Project" 20 t)
           (list "Task" 25 nil)
           (list "Note" 0 nil))
          tabulated-list-entries
          (cl-loop
           for entry in (alist-get 'hourEntries day)
           collect
           (progn
             (let-alist entry
               (list
                .id
                (vector
                 (number-to-string (if (numberp .hours) .hours 0))
                 .projectName
                 (or .taskName "")
                 (or .note "")))))))
    tabulated-list-padding 4
    (use-local-map bhr-timesheet-mode-map)
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (tablist-minor-mode)))

(defun bhr-submit-entries (entries)
  "Submit a list of plist ENTRIES to Bamboo. "
  (bhr--request
   (bhr--url "timesheet/hour/entries")
   :verb "POST"
   :data (json-encode `(:hours ,entries))
   :json t))

(defun bhr-select-task ()
  "Prompt for a task from Bamboo."
  (let ((tasks (bhr--list-tasks))
        (completion-extra-properties
         '(:annotation-function (lambda (cand)
                                  (when-let ((project (map-nested-elt (cdr (assoc cand minibuffer-completion-table)) '(project name))))
                                    (format " (%s)" (string-trim project)))))))
    (cdr (assoc (completing-read "Select a task: " tasks nil t nil bhr--task-history) tasks))))

(defun bhr-submit-multiple ()
  "Submit an entry spanning multiple days."
  (interactive)
  (bhr-ensure-session
   (let ((task (bhr-select-task))
         (days (bhr--get-date-range))
         (hours (read-from-minibuffer "Hours worked for task: " (number-to-string bhr-default-hours)))
         (note (read-from-minibuffer "Add a note for the task (optional) ")))
     (bhr-submit-entries
      (apply
       #'vector
       (cl-loop
        for dailyId = 1 then (+ 1 dailyId)
        for day in days
        collect
        (let ((project (alist-get 'project task)))
          (cl-remove-if
           #'null
           (list (cons 'id nil)
                 (cons 'date day)
                 (cons 'hours hours)
                 (cons 'note note)
                 (cons 'employeeId (alist-get 'employeeId bhr--session-user))
                 (cons 'taskId (when project (alist-get 'id task)))
                 (cons 'projectId (or (alist-get 'id project) (alist-get 'id task)))
                 (cons 'dailyEntryId dailyId))))))))))

(defun bhr-view-timesheet ()
  "View a timesheet interactively."
  (interactive)
  (let ((buffer (get-buffer-create "*bhr-timesheet*")))
    (with-current-buffer buffer
      (bhr-timesheet-mode))
    (pop-to-buffer buffer)))

(provide 'bhr)
;;; bhr.el ends here
