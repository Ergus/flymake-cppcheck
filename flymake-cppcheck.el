(defcustom flymake-cppcheck-executable "cppcheck"
  "cppcheck executable")

(defcustom flymake-cppcheck-arguments '("--enable=all" "--quiet")
  "Command line arguments for cppcheck")

(defconst flymake-cppcheck--template "--template={file}:{line}:{column}:{severity}:{message}"
  "Template format for cppcheck, this is fixed because the parser depend of it")

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
      (kill-buffer (process-buffer process)))))

(defvar-local flymake-cppcheck--executable (executable-find flymake-cppcheck-executable))
(defvar-local flymake-cppcheck--process nil)

(defun flymake-cppcheck--set-connection-locals ()
  "Set connection local variables when possible and needed."
  (when-let* ((remote (file-remote-p default-directory))
	      ((not (local-variable-p 'flymake-cppcheck--executable)))
	      (criteria (connection-local-criteria-for-default-directory))
	      (symvars (intern (format "flymake-cppcheck--%s-vars" remote)))
	      (enable-connection-local-variables t))
    (unless (alist-get symvars connection-local-profile-alist)
      (with-connection-local-variables  ;; because *-executable can be set as connection local
       (let ((cppcheck (if (local-variable-p 'flymake-cppcheck-executable)
			   gtags-mode-global-executable
			 (file-name-nondirectory flymake-cppcheck-executable))))
	 (connection-local-set-profile-variables
	  symvars
	  `((flymake-cppcheck--executable . ,(executable-find cppcheck t))))
	 (connection-local-set-profiles criteria symvars))))
    (hack-connection-local-variables-apply criteria)))

(defun flymake-cppcheck--process-start (report-fn buffer)
  "Flymake backend process for cppcheck."

  (when (process-live-p flymake-cppcheck--process)
    (kill-process flymake-cppcheck--process))
  
  (let ((copy-file (make-temp-file
		    "flymake-cppcheck"
		    nil ;; not a directory
		    (file-name-extension (buffer-file-name buffer) t)
		    (buffer-substring-no-properties (point-min) (point-max)))))
    (condition-case err
	(progn
          (setq-local flymake-cppcheck--process
		      (make-process
		       :name "flymake-cppcheck"
		       :buffer (generate-new-buffer " *cppcheck-output*")
		       :command (flatten-tree
				 `(,flymake-cppcheck--executable
				   ,flymake-cppcheck--template
				   ,flymake-cppcheck-arguments
				   ,copy-file))
		       :noquery t
		       :sentinel #'flymake-cppcheck--sentinel))
	  (process-put flymake-cppcheck--process :flymake-cppcheck-source-file (buffer-file-name buffer))
	  (process-put flymake-cppcheck--process :flymake-cppcheck-copy-file copy-file)
	  (process-put flymake-cppcheck--process :flymake-report-fn report-fn))
      (error
       (delete-file copy-file)
       (signal (car err) (cdr err))))))

(defun flymake-cppcheck-backend (report-fn &rest _args)
  "Flymake backend for cppcheck."
  (flymake-cppcheck--set-connection-locals)
  (when flymake-cppcheck--executable
    (flymake-cppcheck--process-start report-fn (current-buffer))))

(setq-local flymake-diagnostic-functions '(flymake-cppcheck-backend))

;;(add-hook 'flymake-diagnostic-functions 'flymake-cppcheck-backend nil t)
