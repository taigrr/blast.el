;;; blast-test.el --- Tests for blast.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for blast.el utility functions and core logic.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load blast.el from same directory
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "blast" dir)))

;;; Utility tests

(ert-deftest blast-test-is-root-p ()
  "Test filesystem root detection."
  (should (blast--is-root-p "/"))
  (should (blast--is-root-p ""))
  (should (blast--is-root-p "C:/"))
  (should (blast--is-root-p "C:\\"))
  (should-not (blast--is-root-p "/home"))
  (should-not (blast--is-root-p "/tmp/foo")))

(ert-deftest blast-test-normalize-filetype ()
  "Test major-mode to filetype normalization."
  (should (equal (blast--normalize-filetype 'emacs-lisp-mode) "elisp"))
  (should (equal (blast--normalize-filetype 'python-mode) "python"))
  (should (equal (blast--normalize-filetype 'go-mode) "go"))
  (should (equal (blast--normalize-filetype 'js2-mode) "javascript"))
  (should (equal (blast--normalize-filetype 'sh-mode) "bash"))
  (should (equal (blast--normalize-filetype 'c++-mode) "cpp"))
  (should (equal (blast--normalize-filetype 'web-mode) "html"))
  (should (equal (blast--normalize-filetype 'nxml-mode) "xml"))
  (should (equal (blast--normalize-filetype 'conf-toml-mode) "toml"))
  ;; String input
  (should (equal (blast--normalize-filetype "rust-mode") "rust"))
  ;; Symbol without -mode suffix
  (should (equal (blast--normalize-filetype 'fundamental) "fundamental")))

(ert-deftest blast-test-normalize-filetype-passthrough ()
  "Test that unknown modes pass through stripped of -mode."
  (should (equal (blast--normalize-filetype 'zig-mode) "zig"))
  (should (equal (blast--normalize-filetype 'ruby-mode) "ruby"))
  (should (equal (blast--normalize-filetype 'java-mode) "java")))

(ert-deftest blast-test-format-iso8601 ()
  "Test ISO 8601 formatting."
  ;; Use a known epoch timestamp: 2026-01-01T00:00:00Z = 1767225600
  (let ((result (blast--format-iso8601 (seconds-to-time 1767225600))))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z$" result))
    (should (string= result "2026-01-01T00:00:00Z"))))

(ert-deftest blast-test-new-metrics ()
  "Test that new metrics are properly initialized."
  (let ((metrics (blast--new-metrics)))
    (should (= (plist-get metrics :action-count) 0))
    (should (= (plist-get metrics :words-added) 0))
    (should (= (plist-get metrics :lines-added) 0))
    (should (= (plist-get metrics :lines-removed) 0))
    (should (= (plist-get metrics :active-seconds) 0))
    (should-not (plist-get metrics :filetype))
    (should-not (plist-get metrics :filepath))))

(ert-deftest blast-test-get-file-metrics ()
  "Test file metrics creation and retrieval."
  (let ((blast--file-metrics (make-hash-table :test 'equal)))
    ;; First call creates
    (let ((metrics (blast--get-file-metrics "/tmp/test.el" "elisp")))
      (should (equal (plist-get metrics :filepath) "/tmp/test.el"))
      (should (equal (plist-get metrics :filetype) "elisp"))
      (should (= (plist-get metrics :action-count) 0)))
    ;; Second call retrieves same
    (let ((metrics (blast--get-file-metrics "/tmp/test.el" "elisp")))
      (plist-put metrics :action-count 5)
      (should (= (plist-get (gethash "/tmp/test.el" blast--file-metrics) :action-count) 5)))))

(ert-deftest blast-test-reset-flushed-metrics ()
  "Test that flushed metrics are properly reset."
  (let ((blast--file-metrics (make-hash-table :test 'equal)))
    (let ((metrics (blast--get-file-metrics "/tmp/test.el" "elisp")))
      (plist-put metrics :action-count 10)
      (plist-put metrics :words-added 50)
      (plist-put metrics :lines-added 5)
      (plist-put metrics :lines-removed 2)
      (plist-put metrics :active-seconds 120.0))
    (blast--reset-flushed-metrics '("/tmp/test.el"))
    (let ((metrics (gethash "/tmp/test.el" blast--file-metrics)))
      (should (= (plist-get metrics :action-count) 0))
      (should (= (plist-get metrics :words-added) 0))
      (should (= (plist-get metrics :lines-added) 0))
      (should (= (plist-get metrics :lines-removed) 0))
      (should (= (plist-get metrics :active-seconds) 0)))))

(ert-deftest blast-test-make-relative ()
  "Test filepath relativization."
  ;; nil input
  (should-not (blast--make-relative nil))
  ;; Non-git file returns basename
  (let ((temp-file (make-temp-file "blast-test-")))
    (unwind-protect
        (should (equal (blast--make-relative temp-file)
                       (file-name-nondirectory temp-file)))
      (delete-file temp-file))))

(ert-deftest blast-test-project-cache ()
  "Test project cache clearing."
  (let ((blast--project-cache (make-hash-table :test 'equal)))
    (puthash "/tmp/foo/" '(:project "foo") blast--project-cache)
    (should (= (hash-table-count blast--project-cache) 1))
    (blast--clear-project-cache)
    (should (= (hash-table-count blast--project-cache) 0))))

(ert-deftest blast-test-get-project-info-refreshes-branch-from-cache ()
  "Test cached project info still refreshes mutable git branch metadata."
  (let* ((root (make-temp-file "blast-root-" t))
         (git-dir (expand-file-name ".git" root))
         (file (expand-file-name "foo.el" root))
         (blast--project-cache (make-hash-table :test 'equal))
         (branch-calls 0))
    (unwind-protect
        (progn
          (make-directory git-dir t)
          (with-temp-file file
            (insert ";; test"))
          (cl-letf (((symbol-function 'blast--exec)
                     (lambda (command)
                       (cond
                        ((string-match-p "remote get-url origin" command)
                         "git@github.com:taigrr/blast.el.git")
                        ((string-match-p "rev-parse --abbrev-ref HEAD" command)
                         (setq branch-calls (1+ branch-calls))
                         (if (= branch-calls 1) "main" "feature/cached-branch"))
                        (t nil)))))
            (let ((initial (blast--get-project-info file)))
              (should (equal (plist-get initial :project)
                             (file-name-nondirectory (directory-file-name root))))
              (should (equal (plist-get initial :git-branch) "main")))
            (let ((refreshed (blast--get-project-info file)))
              (should (equal (plist-get refreshed :git-branch)
                             "feature/cached-branch")))))
      (delete-directory root t))))

(ert-deftest blast-test-ignored-buffer-no-file ()
  "Test that buffers without files are ignored."
  (with-temp-buffer
    (should (blast--ignored-buffer-p))))

(ert-deftest blast-test-ignored-buffer-special-mode ()
  "Test that special-mode buffers are ignored."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/fake-file.el")
    (let ((major-mode 'special-mode))
      (should (blast--ignored-buffer-p)))))

(ert-deftest blast-test-ignored-buffer-space-prefix ()
  "Test that space-prefixed buffer names are ignored."
  (let ((buf (generate-new-buffer " *hidden*")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-file-name "/tmp/fake.el")
          (should (blast--ignored-buffer-p)))
      (kill-buffer buf))))

(ert-deftest blast-test-find-file-upward ()
  "Test upward file search."
  (let* ((root (make-temp-file "blast-root-" t))
         (sub (expand-file-name "a/b/c" root))
         (target (expand-file-name ".blast.toml" root)))
    (unwind-protect
        (progn
          (make-directory sub t)
          (with-temp-file target
            (insert "name = \"test\""))
          (let ((found (blast--find-file-upward ".blast.toml" sub)))
            (should found)
            (should (string= (expand-file-name found) (expand-file-name target)))))
      (delete-directory root t))))

(ert-deftest blast-test-find-dir-upward ()
  "Test upward directory search."
  (let* ((root (make-temp-file "blast-root-" t))
         (git-dir (expand-file-name ".git" root))
         (sub (expand-file-name "src/lib" root)))
    (unwind-protect
        (progn
          (make-directory git-dir t)
          (make-directory sub t)
          (let ((found (blast--find-dir-upward ".git" sub)))
            (should found)
            (should (string= (expand-file-name found) (expand-file-name git-dir)))))
      (delete-directory root t))))

(ert-deftest blast-test-count-words ()
  "Test word counting in buffer."
  (with-temp-buffer
    (insert "hello world foo bar")
    (should (= (blast--count-words) 4)))
  (with-temp-buffer
    (should (= (blast--count-words) 0))))

(ert-deftest blast-test-session-lifecycle ()
  "Test session start and end without socket."
  (let ((blast--current-session nil)
        (blast--file-metrics (make-hash-table :test 'equal))
        (blast--current-file nil)
        (blast--current-file-entered-at nil)
        (blast--last-word-count 0)
        (blast--last-line-count 0)
        (blast--flush-timer nil)
        (blast--process nil))
    ;; Start session
    (blast--start-session "test-project" "git@github.com:test/repo.git" "elisp" nil "main")
    (should blast--current-session)
    (should (equal (plist-get blast--current-session :project) "test-project"))
    (should (equal (plist-get blast--current-session :git-remote) "git@github.com:test/repo.git"))
    (should (equal (plist-get blast--current-session :git-branch) "main"))
    (should-not (plist-get blast--current-session :private))
    ;; End session (too short, will be discarded)
    (blast--end-session)
    (should-not blast--current-session)
    ;; Stop flush timer if it was started
    (blast--stop-flush-timer)))

(ert-deftest blast-test-private-session ()
  "Test that private sessions mask project info."
  (let ((blast--current-session nil)
        (blast--file-metrics (make-hash-table :test 'equal))
        (blast--current-file nil)
        (blast--current-file-entered-at nil)
        (blast--last-word-count 0)
        (blast--last-line-count 0)
        (blast--flush-timer nil)
        (blast--process nil))
    (blast--start-session "secret-project" "git@github.com:me/secret.git" "go" t "main")
    (should (plist-get blast--current-session :private))
    ;; Add some metrics
    (let ((metrics (blast--get-file-metrics "/tmp/secret.go" "go")))
      (plist-put metrics :action-count 5)
      (plist-put metrics :active-seconds 60.0))
    ;; Build activities — private fields should be masked
    (let ((activities (blast--build-activities)))
      (should (= (length activities) 1))
      (let ((payload (plist-get (car activities) :payload)))
        (should (equal (cdr (assq 'project payload)) "private"))
        (should (equal (cdr (assq 'git_remote payload)) "private"))
        (should (equal (cdr (assq 'git_branch payload)) "private"))
        (should-not (cdr (assq 'filename payload)))))
    (blast--end-session)
    (blast--stop-flush-timer)))

(ert-deftest blast-test-build-activities-empty ()
  "Test building activities with no metrics."
  (let ((blast--current-session (list :project "test" :git-remote nil :git-branch "main"
                                      :started-at (float-time) :private nil))
        (blast--file-metrics (make-hash-table :test 'equal)))
    (should-not (blast--build-activities))))

(ert-deftest blast-test-build-activities-skips-zero ()
  "Test that files with zero activity are skipped."
  (let ((blast--current-session (list :project "test" :git-remote nil :git-branch "main"
                                      :started-at (float-time) :private nil))
        (blast--file-metrics (make-hash-table :test 'equal)))
    ;; File with zero seconds and zero actions
    (blast--get-file-metrics "/tmp/idle.el" "elisp")
    (should-not (blast--build-activities))))

(ert-deftest blast-test-clock-in-out ()
  "Test clock-in and clock-out tracking."
  (let ((blast--file-metrics (make-hash-table :test 'equal))
        (blast--current-file nil)
        (blast--current-file-entered-at nil))
    (blast--get-file-metrics "/tmp/test.el" "elisp")
    (blast--clock-in "/tmp/test.el")
    (should (equal blast--current-file "/tmp/test.el"))
    (should blast--current-file-entered-at)
    ;; Simulate some time passing
    (setq blast--current-file-entered-at (- (float-time) 30))
    (blast--clock-out-current)
    (let ((metrics (gethash "/tmp/test.el" blast--file-metrics)))
      (should (>= (plist-get metrics :active-seconds) 29)))))

(provide 'blast-test)
;;; blast-test.el ends here
