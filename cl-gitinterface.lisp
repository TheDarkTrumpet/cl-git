; To run:
; (require :cl-gitinterface)
; (in-package :cl-gitinterface)

(defpackage :cl-gitinterface
  (:use :cl :cl-ppcre :local-time))

(in-package :cl-gitinterface)


;Available git commands and their associated 

(defparameter *git-commands* (make-hash-table 
			      :pull (make-hash-table 
				     :failure "")
			      :push (make-hash-table
				     :failure ".*?rejected.*")))
			      
;Listen on a stream to for when input comes along.
(defun probe-stream (stream)
  (let ((break-limit (timestamp-to-unix (now))))
    (loop 
       (when (listen stream)
	 (return))
       (when (> (- (timestamp-to-unix (now)) break-limit) 1)
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
(defun cd-into-repo (instream outstream repodir) 
  (format instream "cd ~a~%" repodir)
  (format t "executed cd..~%")
  (force-output instream)
  (format t "forced output .. ~%")
  ;(format t "Output: ~a~%" (get-from-shell outstream))
  (format t "output given..~%"))

;Wrapper for the actual git command.
(defun exec-git-cmd (instream outstream errstream cmd args)
  (format instream "git ~a~%" cmd)
  (force-output instream)
  (format t "Output: ~a~%" (get-from-shell outstream)))

;What the users of this library will use.
(defun git (repodir cmd args)
  (let* ((stream (sb-ext:run-program "/bin/sh" () :output :stream :input :stream :search t :wait nil))
	 (input (sb-ext:process-input stream))
	 (output (sb-ext:process-output stream))
	 (err (sb-ext:process-error stream)))
    (cd-into-repo input output repodir)
    (exec-git-cmd input output err cmd args)
    (format input "exit~%")
    (force-output input)))
