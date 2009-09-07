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

(defmethod print-object ((object git-error) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (text object))))

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
; TODO: do something portable
(defmacro in-directory (dir &body body) 
  "Set the current directory to DIR in BODY"
  (let ((cwd (gensym)))
    `(let ((,cwd (sb-posix:getcwd)))
       (sb-posix:chdir (truename ,dir))
       (prog1 ,@body
         (sb-posix:chdir (truename ,cwd))))))

(defun wait-process (process)
  "Loop until the state of PROCESS isn't :RUNNING anymore"
  (loop for status = (process-status process)
        until (not (eql status :running))))

(defun verify-error (process args error-stream)
  "Wait the end of PROCESS and check the exit-code, raise a GIT-ERROR when 
   the exit-code is different from 0"
  (multiple-value-bind (status exit-code) (process-status process)
    (if (not (eql status :running))
      (when (not (= exit-code 0))
        (error 'git-error
               :cmd (format nil "git ~{~a~}" args)
               :text (get-from-shell error-stream)))
      (progn (wait-process process)
             (verify-error process args error-stream)))))


;What the users of this library will use.
(defun git (repodir &optional (args '()))
  (in-directory repodir
    (let* ((process (start "git" args 
                          :output :stream
                          :error :stream))
           (output (process-output-stream process))
           (err (process-error-stream process)))
      (verify-error process args err)
      (get-from-shell output))))
