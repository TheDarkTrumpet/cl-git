; To run:
; (require :cl-gitinterface)
; (in-package :cl-gitinterface)

(defpackage :cl-gitinterface
  (:use :cl :cl-ppcre :local-time))

(in-package :cl-gitinterface)


;Available git commands and their associated 

;The parent git error class, that the others will inherit from.
(define-condition git-error (error) 
  ((text :initarg :text
	 :reader text)))

(define-condition break-limit-reached (error)
  ((text :initarg :text
	 :reader text)))

; Used when defining a new git command, which will define the condition
; and add it to the list of available commands.
(defmacro define-git-command (cmd)
  `(progn
     (push ,cmd *git-commands*)
     (define-condition ,(intern (format nil "GIT-~A-ERROR" (symbol-name cmd)) :cl-gitinterface)
	 (git-error)())
  ))

; Define the base commands.
(macrolet ((def ()
             `(progn
		(defvar *git-commands* (list))
                ,@(loop for k in '(:pull :push :commit :remote-add :clone :status) collect
		       `(define-git-command ,k)))))
  (def))

;Listen on a stream to for when input comes along.
(defun probe-stream (stream &optional (timeout 60))
  (let ((break-limit (timestamp-to-unix (now))))
    (loop 
       (when (listen stream)
	 (return t))
       (when (> (- (timestamp-to-unix (now)) break-limit) timeout)
	 (error 'break-limit-reached "Time limit reached"))
       )
    ))

;Grab the contents from a shell 
(defun get-from-shell (stream &optional (probe T))
  (if (eql probe T)
      (probe-stream stream))
  (with-output-to-string (out)
    (loop for x = (read-char-no-hang stream nil nil) while x do
	 (format out "~a" x))))

; cd into repository
; Error is : "can't cd to ...." <-- do a regexp on this.
; Success is nothing...that doesn't help.
(defun cd-into-repo (instream repodir) 
  (format instream "cd ~a~%" repodir)
  (force-output instream))

(defun verify-git-error (stroutput cmd)
  (if (not (eql stroutput Nil))
      (error (intern (format nil "GIT-~A-ERROR" (symbol-name cmd))) :text stroutput)
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
	(get-from-shell outstream)
	(format t "after verify ~%")
	(verify-git-error err cmd)
	(format t "Error stream: ~a~%" (get-from-shell err Nil)))
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
