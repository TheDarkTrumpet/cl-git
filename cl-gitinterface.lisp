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
(setf (gethash :pull *git-commands*) ".*?Automatic merge failed; fix conflicts and then commit the result.*")

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
		       `(define-condition ,(intern (concatenate 'string "git-" (string-downcase (symbol-name k)) "-error")) (error)
                       ((text :initarg :text :reader text)))))))
  (def))

;(macrolet ((def ()
;             `(progn
;                ,@(loop for k being the hash-keys in *git-commands* collect 
;                    `(define-condition ,(intern (concatenate 'string "git-" (string-downcase (symbol-name k)) "-error")) (error)
;                       ((text :initarg :text :reader text)))))))
;  (def))

;Loop through all the hash elements and create conditions.
;TODO MAKE MACRO OUT OF THIS!
;(loop for k being the hash-values in *git-commands* do 
;     (let ((v (gethash :errortype k)))       
;       (quote (define-condition v (error)
;	 ((text :initarg :text :reader text))))))

;Listen on a stream to for when input comes along.
(defun probe-stream (stream)
  (let ((break-limit (timestamp-to-unix (now))))
    (loop 
       (when (listen stream)
	 (return))
       (when (> (- (timestamp-to-unix (now)) break-limit) 60)
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
(defun exec-git-cmd (instream outstream cmd args)
  (if (not (eql (gethash cmd *git-commands*) nil))
      (progn
	(format instream "git ~a ~a~%" (string-downcase (symbol-name cmd)) args)
	(force-output instream)
	(let ((x (get-from-shell outstream)))
	  (if (not (eql (scan (gethash cmd *git-commands*) x) nil))
	      (error (gethash :errortype (gethash cmd *git-commands*)) :text x))))
      (error 'invalid-git-command)))

;Create the base stream, set input/output streams as needed.
(defun run-base-sh ()
  (let* ((stream (sb-ext:run-program "/bin/sh" () :output :stream :input :stream :search t :wait nil))
	 (input (sb-ext:process-input stream))
	 (output (sb-ext:process-output stream)))
    (values input output)))

;What the users of this library will use.
(defun git (repodir cmd &optional (args ""))
  (multiple-value-bind (input output) (run-base-sh)
    (cd-into-repo input output repodir)
    (exec-git-cmd input output cmd args)
    (format input "exit~%")
    (force-output input)))
