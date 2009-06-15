; To run:
; (require :cl-gitinterface)
; (in-package :cl-gitinterface)

(defpackage :cl-gitinterface
  (:use :cl :cl-ppcre))

(in-package :cl-gitinterface)

;Listen on a stream to for when input comes along.
(defun probe-stream (stream)
  (loop 
     (when (listen stream)
       (return)))
  )

;Grab the contents from a shell 
(defun get-from-shell (stream)
  (probe-stream stream)
  (with-output-to-string (out)
    (loop for x = (read-char-no-hang stream nil nil) while x do
	 (format out "~a" x))))

;cd into repository
(defun cd-into-repo (instream outstream repodir) 
  (format instream "cd ~a~%" repodir)
  (force-output instream)
  (format t "Output: ~a~%" (get-from-shell outstream)))

;Wrapper for the actual git command.
(defun exec-git-cmd (instream outstream cmd)
  (format instream "git ~a~%" cmd)
  (force-output instream)
  (format t "Output: ~a~%" (get-from-shell outstream)))

;What the users of this library will use.
(defun run-git-cmd (repodir cmd)
  (let* ((stream (sb-ext:run-program "sh" () :output :stream :input :stream :search t :wait nil))
	 (input (sb-ext:process-input stream))
	 (output (sb-ext:process-output stream)))
    (cd-into-repo input output repodir)
    (exec-git-cmd input output cmd)))
