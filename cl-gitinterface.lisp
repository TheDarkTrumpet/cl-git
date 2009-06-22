; To run:
; (require :cl-gitinterface)
; (in-package :cl-gitinterface)

(defpackage :cl-gitinterface
  (:use :cl :cl-ppcre :local-time))

(in-package :cl-gitinterface)


;Available git commands and their associated 

;Empty Hash Table.
;TODO - Don't need such a complex structure of hash commands. 
(defvar *git-commands* (make-hash-table))

;Pull Command.
(setf (gethash :pull *git-commands*) (make-hash-table))
(setf (gethash :failurereg (gethash :pull *git-commands*)) ".*?Automatic merge failed; fix conflicts and then commit the result.*")
(setf (gethash :errortype (gethash :pull *git-commands*)) 'git-merge-conflict)

;Push Command.
(setf (gethash :push *git-commands*) (make-hash-table))
(setf (gethash :failurereg (gethash :push *git-commands*)) ".*?error: failed to push some refs.*")
(setf (gethash :errortype (gethash :push *git-commands*)) 'git-push-conflict)

;Commit Command
(setf (gethash :commit *git-commands*) (make-hash-table))
(setf (gethash :failurereg (gethash :commit *git-commands*)) ".*?fatal: cannot do a partial commit during a merge.*")
(setf (gethash :errortype (gethash :commit *git-commands*)) 'git-commit-error)

;Remote Add
(setf (gethash :remote-add *git-commands*) (make-hash-table))
(setf (gethash :failurereg (gethash :remote-add *git-commands*)) ".*?fatal: remote origin already exists.*")
(setf (gethash :errortype (gethash :remote-add *git-commands*)) 'git-remote-add-error)

;Clone
(setf (gethash :clone *git-commands*) (make-hash-table))
(setf (gethash :failurereg (gethash :clone *git-commands*)) ".*?fatal:.*")
(setf (gethash :errortype (gethash :clone *git-commands*)) 'git-clone-error)

;Status
(setf (gethash :status *git-commands*) (make-hash-table))
(setf (gethash :failurereg (gethash :status *git-commands*)) ".*?fatal:.*")
(setf (gethash :errortype (gethash :status *git-commands*)) 'git-status-error)

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
	  (if (not (eql (scan (gethash :failurereg (gethash cmd *git-commands*)) x) nil))
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
