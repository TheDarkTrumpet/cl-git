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

(defun verify-git-error (stroutput cmd)
  (if (not (eql stroutput Nil))
      (error 'git-error :cmd cmd :text 
             (with-output-to-string (s)
               (format s "Does not exist: ~a" stroutput)))
      T))

;Wrapper for the actual git command.
(defun exec-git-cmd (instream outstream err cmd args)
  (format instream "git ~a ~a~%" cmd args)
  (force-output instream)
  ;(verify-git-error err cmd)
  (get-from-shell outstream)
  )

;Create the base stream, set input/output streams as needed.
(defun run-base-sh ()
  (let* ((stream (sb-ext:run-program "/bin/sh" () 
                                     :output :stream 
                                     :input :stream 
                                     :error :stream 
                                     :search t 
                                     :wait nil))
	 (input (sb-ext:process-input stream))
	 (output (sb-ext:process-output stream))
	 (err (sb-ext:process-error stream)))
    (values input output err)))

;What the users of this library will use.
(defun git (repodir cmd &optional (args ""))
  (multiple-value-bind (input output err) (run-base-sh)
    (cd-into-repo input repodir)
    (prog1 
      (exec-git-cmd input output err cmd args)
      (format input "exit~%")
      (force-output input))))
