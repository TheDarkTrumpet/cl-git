; To run:
; (require :cl-git)
; (in-package :cl-git)

(in-package :cl-git)


;Available git commands and their associated 

;The parent git error class, that the others will inherit from.
(define-condition git-error (error) 
  ((text :initarg :text
	 :reader text)
   (cmd :initarg :cmd
	:reader cmd)))

(define-condition break-limit-reached (error)
  ((text :initarg :text
	 :reader text)))

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

(defun verify-git-error (return-code stroutput cmd)
  (when (not (= return-code 0)) 
    (error 'git-error 
           :cmd cmd 
           :text (with-output-to-string (s)
                   (format s "Does not exist: ~a" stroutput)))))

; return the last return code (echo $?)
(defun get-last-return-code (instream outstream)
  (format instream "echo $?~%")
  (force-output instream)
  (parse-integer (get-from-shell outstream) :junk-allowed t))

;Wrapper for the actual git command.
(defun exec-git-cmd (instream outstream err cmd args)
  (format instream "git ~a ~a~%" cmd args)
  (force-output instream)
  (prog1
    (get-from-shell outstream)
    (verify-git-error (get-last-return-code instream outstream) err cmd)))

;Create the base stream, set input/output streams as needed.
(defun run-base-sh ()
  (let* ((stream (start "/bin/sh" () 
                        :output :stream 
                        :input :stream 
                        :error :stream ))
         (input (process-input-stream stream))
         (output (process-output-stream stream))
         (err (process-error-stream stream)))
    (values input output err)))

;What the users of this library will use.
(defun git (repodir cmd &optional (args ""))
  (multiple-value-bind (input output err) (run-base-sh)
    (cd-into-repo input repodir)
    (prog1 
      (exec-git-cmd input output err cmd args)
      (format input "exit~%")
      (force-output input))))
