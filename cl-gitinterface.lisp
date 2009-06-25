; To run:
; (require :cl-gitinterface)
; (in-package :cl-gitinterface)

(defpackage :cl-gitinterface
  (:use :cl :cl-ppcre :local-time))

(in-package :cl-gitinterface)


;Available git commands and their associated 

;Empty Hash Table.
(defvar *git-commands* (make-hash-table))

;Pull Command.
(setf (gethash :pull *git-commands*) ".*?Automatic merge failed.*")

;Push Command.
(setf (gethash :push *git-commands*) ".*?error: failed to push some refs.*")

;Commit Command
(setf (gethash :commit *git-commands*) ".*?fatal: cannot do a partial commit during a merge.*")

;Remote Add
(setf (gethash :remote-add *git-commands*) ".*?fatal: remote origin already exists.*")

;Clone
(setf (gethash :clone *git-commands*) ".*?fatal:.*")

;Status
(setf (gethash :status *git-commands*) ".*?fatal:.*")

(macrolet ((def ()
             `(progn
                ,@(loop for k in '(:pull :push :commit :remote-add :clone :status) collect 
		       `(define-condition ,(intern (concatenate 'string "GIT-" (symbol-name k) "-ERROR")) (error)((text :initarg :text :reader text))
                       )))))
  (def))

;Listen on a stream to for when input comes along.
(defun probe-stream (stream &optional (timeout 60))
  (let ((break-limit (timestamp-to-unix (now))))
    (loop 
       (when (listen stream)
	 (return))
       (when (> (- (timestamp-to-unix (now)) break-limit) timeout)
	 (error 'break-limit-reached "Time limit reached"))
       )
    ))

;Grab the contents from a shell 
(defun get-from-shell (stream)
  (probe-stream stream)
  (with-output-to-string (out)
    (loop for x = (read-char-no-hang stream nil nil) while x do
	 (format out "~a" x))))

; cd into repository
; Error is : "can't cd to ...." <-- do a regexp on this.
; Success is nothing...that doesn't help.
(defun cd-into-repo (instream repodir) 
  (format instream "cd ~a~%" repodir)
  (force-output instream))

;Verify the output from the cmd and toss an error if it is incorrect.
(defun verify-git-cmd (stroutput cmd)
  (if (not (eql (scan (gethash cmd *git-commands*) stroutput) nil))
      ;(error 'git-pull-error :text stroutput)
      (error (intern (string-upcase (concatenate 'string "git-" (string-downcase (symbol-name cmd )) "-error")) :CL-GITINTERFACE) :text stroutput)
      T))

;Wrapper for the actual git command.
(defun exec-git-cmd (instream outstream err cmd args)
  (if (not (eql (gethash cmd *git-commands*) nil))
      (progn
	(format t "in beginning..~%")
	(format instream "git ~a ~a~%" (string-downcase (symbol-name cmd)) args)
	(format t "doing output..~%")
	(force-output instream)
	(format t "kljlj ~%")
	(verify-git-cmd (get-from-shell outstream) cmd)
	(format t "after verify ~%")
	(format t "Error stream: ~a~%" (get-from-shell err)))
      (error 'invalid-git-command)))

;Create the base stream, set input/output streams as needed.
(defun run-base-sh ()
  (let* ((stream (sb-ext:run-program "/bin/sh" () :output :stream :input :stream :search t :wait nil))
	 (input (sb-ext:process-input stream))
	 (output (sb-ext:process-output stream))
	 (err (sb-ext:process-error stream)))
    (values input output err)))

;What the users of this library will use.
(defun git (repodir cmd &optional (args ""))
  (multiple-value-bind (input output err) (run-base-sh)
    (cd-into-repo input repodir)
    (exec-git-cmd input output err cmd args)
    (format input "exit~%")
    (force-output input)))
