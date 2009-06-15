; To run:
; (require 'git-interface)
; (in-package :git-interface)

(defpackage :git-interface
  (:use :cl :cl-ppcre))

(in-package :git-interface)

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
(defun cd-into-repo (stream repodir) 
  (format stream "cd ~a~%" repodir))

;Wrapper for the actual git command.
(defun exec-git-cmd (stream cmd)
  (format stream "git ~a~%" cmd))

;What the users of this library will use.
(defun run-git-cmd (repodir cmd)
  (let* ((stream (sb-ext:run-program "sh" () :output :stream :input :stream :search t :wait nil))
	 (input (sb-ext:process-input stream))
	 (output (sb-ext:process-output stream)))
    (cd-into-repo input repodir)
    (exec-git-cmd input cmd)))