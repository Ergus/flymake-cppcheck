
(defun flymake-cppcheck--parse-output (output-buffer copy-file orig-file)
  "Parse cppcheck output and call REPORT-FN with diagnostics for SOURCE-BUFFER."
  (with-current-buffer (process-buffer process)
    (let ((diagnostics '()))
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^:]+\\):\\(.*\\)$" nil t)
	(let ((file (match-string 1))
              (line (string-to-number (match-string 2)))
	      (column (string-to-number (match-string 3)))
	      (severity (match-string 4))
              (msg (match-string 5)))
          (when (string= (file-truename file) copy-file)
	    (message "Error at %s %s %s %s" orig-file line column severity)
            (push (flymake-make-diagnostic
                   orig-file
                   (cons line column)
		   nil
                   (pcase severity
		     ("error" :error)
		     ("warning" :warning)
		     (_ :warning))
                   msg)
                  diagnostics))))
      diagnostics)))

(defun flymake-cppcheck--sentinel (process _event)
  (when-let* ((status (process-status process)))
    (when-let* (((eq 'exit status))
		(diagnostics
		 (flymake-cppcheck--parse-output
		  (process-buffer process)
		  (process-get process :flymake-cppcheck-copy-file)
		  (process-get process :flymake-cppcheck-source-file))))
      (funcall (process-get process :flymake-report-fn) diagnostics))
    (when (memq status '(exit signal failed stop))
      (delete-file (process-get process :flymake-cppcheck-copy-file))
      ;; (kill-buffer (process-buffer process))
      )))

(defun flymake-cppcheck--process (report-fn buffer)
  "Flymake backend process for cppcheck.
REPORT-FN is the Flymake reporting function.
SOURCE-BUFFER is the buffer being checked."
  (let* ((copy-file (make-temp-file
		     "flymake-cppcheck"
		     nil ;; not a directory
		     (file-name-extension (buffer-file-name buffer) t)
		     (buffer-substring-no-properties (point-min) (point-max)))))
    (condition-case err
        (let ((proc (make-process
                     :name "flymake-cppcheck"
                     :buffer (generate-new-buffer " *cppcheck-output*")
                     :command (list "cppcheck" "--template={file}:{line}:{column}:{severity}:{message}" "--enable=all" "--quiet" copy-file)
                     :noquery t
                     :connection-type 'pipe
                     :sentinel #'flymake-cppcheck--sentinel
                     )))
	  (process-put proc :flymake-cppcheck-source-file (buffer-file-name buffer))
          (process-put proc :flymake-cppcheck-copy-file copy-file)
	  (process-put proc :flymake-report-fn report-fn))
      (error
       (delete-file copy-file)
       (signal (car err) (cdr err))))))

(defun flymake-cppcheck-backend (report-fn &rest _args)
  "Flymake backend for cppcheck.
REPORT-FN is the Flymake reporting function."
  (flymake-cppcheck--process report-fn (current-buffer)))

(setq-local flymake-diagnostic-functions '(flymake-cppcheck-backend))

;;(add-hook 'flymake-diagnostic-functions 'flymake-cppcheck-backend nil t)
