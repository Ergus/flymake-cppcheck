;;; flymake-cppcheck.el --- Cppcheck integration with flymake. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/gtags-mode
;; Keywords: cppcheck flymake
;; Version: 0.1
;; Package-Requires: ((emacs "28"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Cppcheck integration with Flymake.

;; This attempt to integrate cppcheck with flymake with special care
;; to tramp and remote systems.

;; One of the goals is to integrate also with project and
;; project-multi packages.

;;; Code:

(require 'flymake)
(require 'project)

(defgroup flymake-cppcheck nil
  "Cppcheck for flymake."
  :group 'tools)

(defcustom flymake-cppcheck-executable "cppcheck"
  "cppcheck executable"
  :type 'string)

(defcustom flymake-cppcheck-arguments '("--enable=all" "--quiet")
  "Command line arguments for cppcheck"
  :type '(repeat string))

(defconst flymake-cppcheck--template "--template={file}:{line}:{column}:{severity}:{message}"
  "Template format for cppcheck, this is fixed because the parser depend of it")

(defun flymake-cppcheck--parse-output (output-buffer file-name)
  "Parse cppcheck output and call REPORT-FN with diagnostics for SOURCE-BUFFER."
  (with-current-buffer output-buffer
    (let ((diagnostics '())
	  (local-name (file-local-name file-name)))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^:]+\\):\\(.*\\)$" nil t)
	(when (and (file-name-absolute-p (match-string-no-properties 1))
		   (string= (match-string-no-properties 1) local-name))
	  (let ((line (string-to-number (match-string-no-properties 2)))
		(column (string-to-number (match-string-no-properties 3)))
		(severity (match-string-no-properties 4))
		(msg (match-string-no-properties 5)))
	    (push (flymake-make-diagnostic
		   file-name
		   (cons line column)
		   nil
		   (cond
		    ((string-equal severity "error") :error)
		    ((string-equal severity "warning") :warning)
		    (t :warning))
		   msg)
		  diagnostics))))
      diagnostics)))

(defun flymake-cppcheck--sentinel (process _event)
  "Sentinel to handle cppcheck process end."
  (when-let* ((status (process-status process)))
    (when-let* (((eq 'exit status))
		(diagnostics
		 (flymake-cppcheck--parse-output
		  (process-buffer process)
		  (process-get process :flymake-cppcheck-source-file))))
      (funcall (process-get process :flymake-report-fn) diagnostics))
    ;; (when (memq status '(exit signal failed stop))
    ;;	 ;; (kill-buffer (process-buffer process))
    ;;	 )
    ))

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
      (with-connection-local-variables	;; because *-executable can be set as connection local
       (let ((cppcheck (if (local-variable-p 'flymake-cppcheck-executable)
			   gtags-mode-global-executable
			 (file-name-nondirectory flymake-cppcheck-executable))))
	 (connection-local-set-profile-variables
	  symvars
	  `((flymake-cppcheck--executable . ,(executable-find cppcheck t))))
	 (connection-local-set-profiles criteria symvars))))
    (hack-connection-local-variables-apply criteria)))

(defun flymake-cppcheck--process-start-file (report-fn buffer process-buffer)
  "Flymake backend process for cppcheck."
  (condition-case err
      (progn
	(setq-local flymake-cppcheck--process
		    (make-process
		     :name "flymake-cppcheck-file"
		     :buffer process-buffer
		     :command (flatten-tree
			       `(,flymake-cppcheck--executable
				 ,flymake-cppcheck--template
				 ,flymake-cppcheck-arguments
				 ,(file-local-name (buffer-file-name buffer))))
		     :noquery t
		     :file-handler t
		     :sentinel #'flymake-cppcheck--sentinel))
	(process-put flymake-cppcheck--process :flymake-cppcheck-source-file (buffer-file-name buffer))
	(process-put flymake-cppcheck--process :flymake-report-fn report-fn))
    (error
     (delete-file copy-file)
     (signal (car err) (cdr err)))))


(defvar-local flymake-cppcheck--process-build-dir nil)

(defun flymake-cppcheck--process-start-project (report-fn buffer process-buffer)
  (when-let* ((project (project-current))
	      (build-database (project--get-extra-info project :compile-database))
	      (default-directory (project-root project)))

    (unless flymake-cppcheck--process-build-dir
      (setq flymake-cppcheck--process-build-dir
	    (expand-file-name "cppcheck-build-dir"
			      (project--get-extra-info project :build-dir)))
      (unless (file-readable-p flymake-cppcheck--process-build-dir)
	(make-directory flymake-cppcheck--process-build-dir)))

    (condition-case err
	(progn
	  (setq-local flymake-cppcheck--process
		      (make-process
		       :name "flymake-cppcheck-project"
		       :buffer process-buffer
		       :command (flatten-tree
				 `(,flymake-cppcheck--executable
				   ,flymake-cppcheck--template
				   ,flymake-cppcheck-arguments
				   ,(concat "--cppcheck-build-dir="
					    (file-local-name flymake-cppcheck--process-build-dir))
				   ,(concat "--project="
					    (file-local-name build-database))))
		       :noquery t
		       :file-handler t
		       :sentinel #'flymake-cppcheck--sentinel))
	  (process-put flymake-cppcheck--process :flymake-cppcheck-source-file (buffer-file-name buffer))
	  (process-put flymake-cppcheck--process :flymake-report-fn report-fn))
      (error
       (delete-file copy-file)
       (signal (car err) (cdr err))))))

(defun flymake-cppcheck-backend (report-fn &rest _args)
  "Flymake backend for cppcheck.
This call performs multiple actions:
1. Stop the previous process if it was running.
2. Clear the process buffer (better here to debug)
3. Attempts to call cppcheck at project level (if possible)
4. Else, run cppcheck only for this buffer."
  (flymake-cppcheck--set-connection-locals)
  (when flymake-cppcheck--executable

    (when (process-live-p flymake-cppcheck--process)
      (kill-process flymake-cppcheck--process))
    (let ((process-buffer (get-buffer-create " *cppcheck-output*" t)))
      (with-current-buffer process-buffer
	(erase-buffer))
      (or (flymake-cppcheck--process-start-project report-fn (current-buffer) process-buffer)
	  (flymake-cppcheck--process-start-file report-fn (current-buffer) process-buffer)))))

;;;###autoload
(define-minor-mode flymake-cppcheck-mode
  "Use cppcheck as backend flymake."
  :global nil
  (if flymake-cppcheck-mode
      (progn
	(unless flymake-mode
	  (flymake-mode 1))
	(add-hook 'flymake-diagnostic-functions #'flymake-cppcheck-backend))
    (remove-hook 'flymake-diagnostic-functions #'flymake-cppcheck-backend)))

(provide 'flymake-cppcheck)
;;; flymake-cppcheck.el ends here

