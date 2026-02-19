;;; blast.el --- Track coding activity and send to blastd daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tai Groot

;; Author: Tai Groot
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, productivity
;; URL: https://github.com/taigrr/blast.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; blast.el tracks coding activity (time, filetypes, APM, WPM) and sends it to
;; a local blastd daemon via Unix socket.  The daemon syncs data to the Blast
;; web dashboard.
;;
;; Requirements: Emacs 27.1+, blastd daemon (https://github.com/taigrr/blastd)
;;
;; Usage:
;;   (require 'blast)
;;   (blast-mode 1)
;;
;; Commands:
;;   M-x blast-status - Show socket connection and session info
;;   M-x blast-ping   - Ping the blastd daemon
;;   M-x blast-sync   - Trigger immediate sync to Blast server

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup blast nil
  "Track coding activity and send to blastd daemon."
  :group 'tools
  :prefix "blast-")

(defcustom blast-socket-path
  (expand-file-name "~/.local/share/blastd/blastd.sock")
  "Path to the blastd Unix socket."
  :type 'string
  :group 'blast)

(defcustom blast-idle-timeout 120
  "Seconds of inactivity before ending a session."
  :type 'integer
  :group 'blast)

(defcustom blast-debounce-ms 1000
  "Debounce interval in milliseconds for word count updates."
  :type 'integer
  :group 'blast)

(defcustom blast-debug nil
  "Enable debug messages."
  :type 'boolean
  :group 'blast)

(defcustom blast-ignored-major-modes '(dired-mode special-mode)
  "Major modes to ignore for tracking."
  :type '(repeat symbol)
  :group 'blast)

;;; Internal variables

(defvar blast--process nil
  "Network process for blastd connection.")

(defvar blast--ping-timer nil
  "Timer for keepalive pings.")

(defvar blast--idle-timer nil
  "Timer for idle detection.")

(defvar blast--debounce-timer nil
  "Timer for debounced word counting.")

(defvar blast--flush-timer nil
  "Timer for periodic activity flushing.")

(defvar blast--current-session nil
  "Current session plist: project, git-remote, git-branch, started-at, private.")

(defvar blast--current-file nil
  "Current file being tracked.")

(defvar blast--current-file-entered-at nil
  "Timestamp when current file was entered.")

(defvar blast--file-metrics (make-hash-table :test 'equal)
  "Hash table of file path -> metrics plist.")

(defvar blast--last-word-count 0
  "Word count from last measurement.")

(defvar blast--last-line-count 0
  "Line count from last measurement.")

(defvar blast--last-activity 0
  "Timestamp of last activity.")

(defvar blast--project-cache (make-hash-table :test 'equal)
  "Cache of directory -> project info.")

(defvar blast--initialized nil
  "Whether blast has been initialized.")

(defvar blast--pending-callbacks nil
  "Alist of request processes to callbacks for sync requests.")

;;; Utilities

(defun blast--debug (format-string &rest args)
  "Log debug message if `blast-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when blast-debug
    (apply #'message (concat "[blast.el] " format-string) args)))

(defun blast--is-root-p (dir)
  "Return non-nil if DIR is a filesystem root."
  (or (string= dir "/")
      (string= dir "")
      (string-match-p "^[a-zA-Z]:[/\\\\]?$" dir)))

(defun blast--find-file-upward (filename start-dir &optional stop-dir)
  "Search for FILENAME starting from START-DIR going upward.
Stop at STOP-DIR if provided."
  (let ((dir (expand-file-name start-dir)))
    (catch 'found
      (while (not (blast--is-root-p dir))
        (let ((path (expand-file-name filename dir)))
          (when (file-readable-p path)
            (throw 'found path)))
        (when (and stop-dir (string= dir stop-dir))
          (throw 'found nil))
        (let ((parent (file-name-directory (directory-file-name dir))))
          (when (string= parent dir)
            (throw 'found nil))
          (setq dir parent)))
      nil)))

(defun blast--find-dir-upward (dirname start-dir)
  "Search for directory DIRNAME starting from START-DIR going upward."
  (let ((dir (expand-file-name start-dir)))
    (catch 'found
      (while (not (blast--is-root-p dir))
        (let ((path (expand-file-name dirname dir)))
          (when (file-directory-p path)
            (throw 'found path)))
        (let ((parent (file-name-directory (directory-file-name dir))))
          (when (string= parent dir)
            (throw 'found nil))
          (setq dir parent)))
      nil)))

(defun blast--get-git-root (filepath)
  "Get the git root directory for FILEPATH."
  (let* ((dir (file-name-directory (expand-file-name filepath)))
         (git-dir (blast--find-dir-upward ".git" dir)))
    (when git-dir
      (file-name-directory (directory-file-name git-dir)))))

(defun blast--exec (command)
  "Execute COMMAND and return trimmed output."
  (let ((result (shell-command-to-string command)))
    (when (and result (not (string= result "")))
      (string-trim result))))

(defun blast--read-file (path)
  "Read and return contents of file at PATH."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun blast--get-project-info (filepath)
  "Get project info for FILEPATH.
Returns a plist with :project, :git-remote, :git-branch, :private."
  (let* ((dir (file-name-directory (expand-file-name filepath)))
         (cached (gethash dir blast--project-cache)))
    (if cached
        cached
      (let* ((git-dir (blast--find-dir-upward ".git" dir))
             (git-root (when git-dir
                         (file-name-directory (directory-file-name git-dir))))
             (blast-config (blast--find-file-upward ".blast.toml" dir git-root))
             (project nil)
             (git-remote nil)
             (git-branch nil)
             (private nil))
        ;; Parse .blast.toml if present
        (when blast-config
          (let ((content (blast--read-file blast-config)))
            (when content
              (when (string-match "name\\s-*=\\s-*\"\\([^\"]+\\)\"" content)
                (setq project (match-string 1 content)))
              (when (string-match "private\\s-*=\\s-*true" content)
                (setq private t)))))
        ;; Get git info
        (when git-root
          (unless project
            (setq project (file-name-nondirectory
                           (directory-file-name git-root))))
          (let ((remote (blast--exec
                         (format "git -C %s remote get-url origin 2>/dev/null"
                                 (shell-quote-argument git-root)))))
            (when (and remote (not (string= remote "")))
              (setq git-remote remote)))
          (let ((branch (blast--exec
                         (format "git -C %s rev-parse --abbrev-ref HEAD 2>/dev/null"
                                 (shell-quote-argument git-root)))))
            (when (and branch (not (string= branch "")))
              (setq git-branch branch))))
        ;; Fallback project name
        (unless project
          (setq project (file-name-nondirectory (directory-file-name dir))))
        ;; Cache and return
        (let ((info (list :project project
                          :git-remote git-remote
                          :git-branch git-branch
                          :private private)))
          (puthash dir info blast--project-cache)
          info)))))

(defun blast--make-relative (filepath)
  "Make FILEPATH relative to git root or return basename."
  (if filepath
      (let ((git-root (blast--get-git-root filepath)))
        (if git-root
            (let ((rel (substring filepath (length git-root))))
              (if (string= rel "") (file-name-nondirectory filepath) rel))
          (file-name-nondirectory filepath)))
    filepath))

(defun blast--count-words ()
  "Count words in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (forward-word 1)
        (cl-incf count))
      count)))

(defun blast--find-blastd-bin ()
  "Find the blastd binary in PATH."
  (executable-find "blastd"))

(defun blast--format-iso8601 (time)
  "Format TIME as ISO 8601 UTC string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

;;; Socket connection

(defun blast--socket-alive-p (socket-path)
  "Check if socket at SOCKET-PATH is alive by attempting connection."
  (when (file-exists-p socket-path)
    (condition-case nil
        (let ((proc (make-network-process
                     :name "blast-probe"
                     :family 'local
                     :service socket-path
                     :noquery t)))
          (when proc
            (delete-process proc)
            t))
      (error nil))))

(defun blast--ensure-blastd ()
  "Ensure blastd daemon is running, starting it if needed."
  (unless (file-exists-p blast-socket-path)
    (let ((bin (blast--find-blastd-bin)))
      (if bin
          (progn
            (blast--debug "Starting blastd: %s" bin)
            (let ((process (start-process "blastd" nil bin)))
              (set-process-query-on-exit-flag process nil))
            ;; Wait for socket to appear
            (let ((waited 0))
              (while (and (< waited 500)
                          (not (file-exists-p blast-socket-path)))
                (sleep-for 0.05)
                (cl-incf waited 50))))
        (blast--debug "blastd not found in PATH")))))

(defun blast--connect ()
  "Connect to blastd socket."
  (when (and blast--process (process-live-p blast--process))
    (return-from blast--connect t))
  (blast--ensure-blastd)
  (condition-case err
      (progn
        (setq blast--process
              (make-network-process
               :name "blast"
               :family 'local
               :service blast-socket-path
               :noquery t
               :filter #'blast--process-filter
               :sentinel #'blast--process-sentinel))
        (blast--debug "Connected to blastd")
        t)
    (error
     (blast--debug "Connection failed: %s" (error-message-string err))
     (setq blast--process nil)
     nil)))

(defun blast--process-filter (process output)
  "Handle OUTPUT from PROCESS."
  (blast--debug "Received: %s" (string-trim output)))

(defun blast--process-sentinel (process event)
  "Handle PROCESS state change EVENT."
  (when (string-match-p "\\(deleted\\|connection broken\\|failed\\)" event)
    (blast--debug "Connection closed: %s" (string-trim event))
    (setq blast--process nil)))

(defun blast--disconnect ()
  "Disconnect from blastd socket."
  (when blast--process
    (ignore-errors (delete-process blast--process))
    (setq blast--process nil)))

(defun blast--connected-p ()
  "Return non-nil if connected to blastd."
  (and blast--process (process-live-p blast--process)))

(defun blast--send (data)
  "Send DATA as JSON to blastd.  Returns t on success."
  (unless (blast--connect)
    (return-from blast--send nil))
  (condition-case err
      (let ((json-str (concat (json-encode data) "\n")))
        (process-send-string blast--process json-str)
        t)
    (error
     (blast--debug "Send failed: %s" (error-message-string err))
     (blast--disconnect)
     nil)))

(defun blast--request (data callback)
  "Send DATA as JSON and call CALLBACK with (ok result) when response arrives.
Opens a dedicated connection for request-response."
  (blast--ensure-blastd)
  (condition-case err
      (let* ((buffer "")
             (proc (make-network-process
                    :name "blast-request"
                    :family 'local
                    :service blast-socket-path
                    :noquery t
                    :filter (lambda (proc output)
                              (setq buffer (concat buffer output))
                              (when (string-match "^\\([^\n]+\\)\n" buffer)
                                (let* ((line (match-string 1 buffer))
                                       (resp (ignore-errors (json-read-from-string line))))
                                  (delete-process proc)
                                  (if resp
                                      (if (eq (cdr (assq 'ok resp)) t)
                                          (funcall callback t (or (cdr (assq 'message resp)) "ok"))
                                        (funcall callback nil (or (cdr (assq 'error resp)) "unknown error")))
                                    (funcall callback nil "invalid response")))))
                    :sentinel (lambda (proc event)
                                (when (string-match-p "failed\\|broken" event)
                                  (funcall callback nil (format "connection error: %s" event)))))))
        (process-send-string proc (concat (json-encode data) "\n")))
    (error
     (funcall callback nil (format "request failed: %s" (error-message-string err))))))

(defun blast--ping ()
  "Send ping to blastd."
  (blast--send '((type . "ping"))))

(defun blast--send-activity (activity)
  "Send ACTIVITY data to blastd."
  (blast--send `((type . "activity") (data . ,activity))))

;;; Metrics tracking

(defun blast--new-metrics ()
  "Create a new metrics plist."
  (list :action-count 0
        :words-added 0
        :lines-added 0
        :lines-removed 0
        :active-seconds 0
        :filetype nil
        :filepath nil))

(defun blast--get-file-metrics (filepath filetype)
  "Get or create metrics for FILEPATH with FILETYPE."
  (let ((metrics (gethash filepath blast--file-metrics)))
    (unless metrics
      (setq metrics (blast--new-metrics))
      (puthash filepath metrics blast--file-metrics))
    (plist-put metrics :filepath filepath)
    (when (and filetype (not (string= filetype "")))
      (plist-put metrics :filetype filetype))
    metrics))

(defun blast--clock-out-current ()
  "Record time spent in current file."
  (when (and blast--current-file blast--current-file-entered-at)
    (let ((metrics (gethash blast--current-file blast--file-metrics)))
      (when metrics
        (let ((elapsed (- (float-time) blast--current-file-entered-at)))
          (when (> elapsed 0)
            (plist-put metrics :active-seconds
                       (+ (plist-get metrics :active-seconds) elapsed))))))
    (setq blast--current-file-entered-at nil)))

(defun blast--clock-in (filepath)
  "Start tracking time for FILEPATH."
  (setq blast--current-file filepath)
  (setq blast--current-file-entered-at (float-time)))

;;; Session management

(defun blast--build-activities ()
  "Build list of activity payloads from current metrics."
  (unless blast--current-session
    (return-from blast--build-activities nil))
  (let* ((session blast--current-session)
         (project-name (if (plist-get session :private)
                           "private"
                         (plist-get session :project)))
         (remote (if (plist-get session :private)
                     "private"
                   (plist-get session :git-remote)))
         (branch (if (plist-get session :private)
                     "private"
                   (plist-get session :git-branch)))
         (now (float-time))
         (activities nil))
    (maphash
     (lambda (key metrics)
       (let ((seconds (plist-get metrics :active-seconds))
             (action-count (plist-get metrics :action-count)))
         (when (or (>= seconds 1) (> action-count 0))
           (when (< seconds 1) (setq seconds 1))
           (let* ((minutes (/ seconds 60.0))
                  (apm (if (> minutes 0) (/ action-count minutes) 0))
                  (wpm (if (> minutes 0) (/ (plist-get metrics :words-added) minutes) 0))
                  (filename (unless (plist-get session :private)
                              (blast--make-relative (plist-get metrics :filepath))))
                  (started-at (- now seconds)))
             (push (list :key key
                         :payload `((project . ,project-name)
                                    (git_remote . ,remote)
                                    (git_branch . ,branch)
                                    (started_at . ,(blast--format-iso8601 started-at))
                                    (ended_at . ,(blast--format-iso8601 now))
                                    (filename . ,filename)
                                    (filetype . ,(plist-get metrics :filetype))
                                    (lines_added . ,(plist-get metrics :lines-added))
                                    (lines_removed . ,(plist-get metrics :lines-removed))
                                    (actions_per_minute . ,(/ (round (* apm 10)) 10.0))
                                    (words_per_minute . ,(/ (round (* wpm 10)) 10.0))
                                    (editor . "emacs")))
                   activities)))))
     blast--file-metrics)
    activities))

(defun blast--reset-flushed-metrics (keys)
  "Reset metrics for KEYS after flushing."
  (dolist (key keys)
    (let ((metrics (gethash key blast--file-metrics)))
      (when metrics
        (plist-put metrics :action-count 0)
        (plist-put metrics :words-added 0)
        (plist-put metrics :lines-added 0)
        (plist-put metrics :lines-removed 0)
        (plist-put metrics :active-seconds 0)))))

(defun blast--flush ()
  "Flush current metrics to blastd."
  (unless blast--current-session
    (return-from blast--flush nil))
  (blast--clock-out-current)
  (let ((activities (blast--build-activities)))
    (when activities
      (let ((keys (mapcar (lambda (a) (plist-get a :key)) activities)))
        (dolist (activity activities)
          (blast--send-activity (plist-get activity :payload)))
        (blast--reset-flushed-metrics keys)
        (blast--debug "Flushed %d file activities" (length activities)))))
  (when blast--current-file
    (blast--clock-in blast--current-file)))

(defun blast--start-flush-timer ()
  "Start the periodic flush timer."
  (blast--stop-flush-timer)
  (setq blast--flush-timer
        (run-at-time 60 60 #'blast--flush)))

(defun blast--stop-flush-timer ()
  "Stop the periodic flush timer."
  (when blast--flush-timer
    (cancel-timer blast--flush-timer)
    (setq blast--flush-timer nil)))

(defun blast--start-session (project git-remote filetype private git-branch)
  "Start a new session for PROJECT with GIT-REMOTE, FILETYPE, PRIVATE, GIT-BRANCH."
  (setq blast--current-session
        (list :project project
              :git-remote git-remote
              :git-branch git-branch
              :started-at (float-time)
              :private private))
  (clrhash blast--file-metrics)
  (setq blast--current-file nil)
  (setq blast--current-file-entered-at nil)
  (setq blast--last-word-count 0)
  (setq blast--last-line-count 0)
  ;; Initialize for current buffer
  (when (buffer-file-name)
    (blast--get-file-metrics (buffer-file-name) filetype)
    (blast--clock-in (buffer-file-name))
    (setq blast--last-word-count (blast--count-words))
    (setq blast--last-line-count (count-lines (point-min) (point-max))))
  (blast--start-flush-timer)
  (blast--debug "Started session: %s" (or project "unknown")))

(defun blast--end-session ()
  "End the current session and send final metrics."
  (unless blast--current-session
    (return-from blast--end-session nil))
  (let* ((session blast--current-session)
         (duration (- (float-time) (plist-get session :started-at))))
    (setq blast--current-session nil)
    (blast--stop-flush-timer)
    ;; Discard short sessions
    (when (< duration 10)
      (clrhash blast--file-metrics)
      (setq blast--current-file nil)
      (setq blast--current-file-entered-at nil)
      (setq blast--last-word-count 0)
      (setq blast--last-line-count 0)
      (return-from blast--end-session nil))
    (blast--clock-out-current)
    (let ((activities (blast--build-activities)))
      (dolist (activity activities)
        (blast--send-activity (plist-get activity :payload)))
      (blast--debug "Ended session: %s (%ds, %d files)%s"
                    (or (plist-get session :project) "unknown")
                    (round duration)
                    (length activities)
                    (if (plist-get session :private) " [private]" "")))
    (clrhash blast--file-metrics)
    (setq blast--current-file nil)
    (setq blast--current-file-entered-at nil)
    (setq blast--last-word-count 0)
    (setq blast--last-line-count 0)))

;;; Activity tracking hooks

(defun blast--ignored-buffer-p ()
  "Return non-nil if current buffer should be ignored."
  (or (not (buffer-file-name))
      (string= (buffer-name) " ")
      (string-prefix-p " *" (buffer-name))
      (memq major-mode blast-ignored-major-modes)))

(defun blast--on-buffer-activity ()
  "Handle buffer enter/save activity."
  (when (blast--ignored-buffer-p)
    (return-from blast--on-buffer-activity nil))
  (let* ((filepath (buffer-file-name))
         (filetype (symbol-name major-mode))
         (info (blast--get-project-info filepath))
         (project (plist-get info :project))
         (git-remote (plist-get info :git-remote))
         (git-branch (plist-get info :git-branch))
         (private (plist-get info :private)))
    (setq blast--last-activity (float-time))
    ;; Switch session if project changed
    (when (or (not blast--current-session)
              (not (string= (plist-get blast--current-session :project) project)))
      (blast--end-session)
      (blast--start-session project git-remote filetype private git-branch))
    ;; Switch file
    (when (not (string= filepath blast--current-file))
      (blast--flush)
      (blast--get-file-metrics filepath filetype)
      (blast--clock-in filepath))
    (setq blast--last-word-count (blast--count-words))
    (setq blast--last-line-count (count-lines (point-min) (point-max)))
    (blast--reset-idle-timer)))

(defun blast--on-text-change ()
  "Handle text change activity."
  (when (blast--ignored-buffer-p)
    (return-from blast--on-text-change nil))
  (setq blast--last-activity (float-time))
  (let* ((filepath (buffer-file-name))
         (filetype (symbol-name major-mode))
         (metrics (blast--get-file-metrics filepath filetype)))
    ;; Increment action count
    (plist-put metrics :action-count (1+ (plist-get metrics :action-count)))
    ;; Handle file switch
    (when (not (string= filepath blast--current-file))
      (blast--clock-out-current)
      (blast--clock-in filepath))
    ;; Debounced word/line counting
    (when blast--debounce-timer
      (cancel-timer blast--debounce-timer))
    (setq blast--debounce-timer
          (run-at-time (/ blast-debounce-ms 1000.0) nil
                       (lambda ()
                         (when (and (buffer-live-p (current-buffer))
                                    (buffer-file-name)
                                    (string= (buffer-file-name) filepath))
                           (let ((new-words (blast--count-words))
                                 (new-lines (count-lines (point-min) (point-max))))
                             (let ((word-delta (- new-words blast--last-word-count)))
                               (when (> word-delta 0)
                                 (plist-put metrics :words-added
                                            (+ (plist-get metrics :words-added) word-delta))))
                             (let ((line-delta (- new-lines blast--last-line-count)))
                               (cond
                                ((> line-delta 0)
                                 (plist-put metrics :lines-added
                                            (+ (plist-get metrics :lines-added) line-delta)))
                                ((< line-delta 0)
                                 (plist-put metrics :lines-removed
                                            (+ (plist-get metrics :lines-removed) (abs line-delta))))))
                             (setq blast--last-word-count new-words)
                             (setq blast--last-line-count new-lines))))))
    (blast--reset-idle-timer)))

;;; Timers

(defun blast--start-keepalive ()
  "Start the keepalive ping timer."
  (blast--stop-keepalive)
  (setq blast--ping-timer
        (run-at-time 10 10 #'blast--ping)))

(defun blast--stop-keepalive ()
  "Stop the keepalive ping timer."
  (when blast--ping-timer
    (cancel-timer blast--ping-timer)
    (setq blast--ping-timer nil)))

(defun blast--reset-idle-timer ()
  "Reset the idle timeout timer."
  (when blast--idle-timer
    (cancel-timer blast--idle-timer))
  (setq blast--idle-timer
        (run-at-time blast-idle-timeout nil
                     (lambda ()
                       (when (and blast--current-session
                                  (>= (- (float-time) blast--last-activity)
                                      blast-idle-timeout))
                         (blast--end-session))))))

;;; Public interface

;;;###autoload
(defun blast-status ()
  "Display blast.el status information."
  (interactive)
  (let ((connected (blast--connected-p))
        (session blast--current-session))
    (message "blast.el status:
  Socket: %s
  Socket path: %s
  %s"
             (if connected "connected" "disconnected")
             blast-socket-path
             (if session
                 (format "Current session: %s
  Filetype: %s
  Files: %d
  Duration: %ds"
                         (or (plist-get session :project) "unknown")
                         (when blast--current-file
                           (let ((m (gethash blast--current-file blast--file-metrics)))
                             (when m (plist-get m :filetype))))
                         (hash-table-count blast--file-metrics)
                         (round (- (float-time) (plist-get session :started-at))))
               "No active session"))))

;;;###autoload
(defun blast-ping ()
  "Ping the blastd daemon."
  (interactive)
  (if (blast--ping)
      (message "[blast.el] pong!")
    (message "[blast.el] ping failed")))

;;;###autoload
(defun blast-sync ()
  "Trigger immediate sync to Blast server."
  (interactive)
  (message "[blast.el] syncing...")
  (blast--request
   '((type . "sync"))
   (lambda (ok result)
     (if ok
         (message "[blast.el] %s" result)
       (message "[blast.el] sync failed: %s" result)))))

(defun blast--setup-hooks ()
  "Set up activity tracking hooks."
  (add-hook 'find-file-hook #'blast--on-buffer-activity)
  (add-hook 'after-save-hook #'blast--on-buffer-activity)
  (add-hook 'window-buffer-change-functions
            (lambda (_) (blast--on-buffer-activity)))
  (add-hook 'after-change-functions
            (lambda (&rest _) (blast--on-text-change)))
  (add-hook 'kill-emacs-hook #'blast--end-session))

(defun blast--remove-hooks ()
  "Remove activity tracking hooks."
  (remove-hook 'find-file-hook #'blast--on-buffer-activity)
  (remove-hook 'after-save-hook #'blast--on-buffer-activity)
  (remove-hook 'window-buffer-change-functions
               (lambda (_) (blast--on-buffer-activity)))
  (remove-hook 'after-change-functions
               (lambda (&rest _) (blast--on-text-change)))
  (remove-hook 'kill-emacs-hook #'blast--end-session))

(defun blast--shutdown ()
  "Clean up blast resources."
  (blast--end-session)
  (blast--stop-keepalive)
  (blast--stop-flush-timer)
  (when blast--idle-timer
    (cancel-timer blast--idle-timer)
    (setq blast--idle-timer nil))
  (when blast--debounce-timer
    (cancel-timer blast--debounce-timer)
    (setq blast--debounce-timer nil))
  (blast--disconnect)
  (blast--remove-hooks)
  (setq blast--initialized nil))

;;;###autoload
(define-minor-mode blast-mode
  "Toggle blast.el activity tracking mode.
When enabled, tracks coding activity and sends to blastd daemon."
  :global t
  :lighter " Blast"
  :group 'blast
  (if blast-mode
      (unless blast--initialized
        (setq blast--initialized t)
        (blast--setup-hooks)
        (blast--connect)
        (blast--start-keepalive)
        (blast--on-buffer-activity)
        (blast--debug "Initialized"))
    (blast--shutdown)))

(provide 'blast)
;;; blast.el ends here
