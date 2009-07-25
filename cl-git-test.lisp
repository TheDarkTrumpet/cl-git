; To run:
; (require :cl-gitinterface)
; (in-package :cl-gitinterface)
; (load "/this/path/cl-gitinterface-test.lisp")
; (run-tests)

(eval-when (:compile-toplevel :load-toplevel
			      :execute) (require :cl-gitinterface) (require :sb-posix) (require :lisp-unit))

(defpackage :cl-gitinterface-test
  (:use :cl :cl-gitinterface :lisp-unit))

;(use-package :lisp-unit)
(in-package :cl-gitinterface-test)

(defvar *cmd-cur* "")
(defvar *cmd-mode* "error")

;Stub out the get-from-shell function, since we want to test through the use of errors.
;(defun cl-gitinterface::get-from-shell (stream)
;  (declare (ignore stream))
;  (format t "in new git-from-shell: ~a~%" *cmd-cur*)
;  (let ((retvar ""))
;    (cond
;      ((eql *cmd-cur* "pull")
;       (setf retvar "repository clean!")))
;    retvar))

;Stub out the run-base-sh from the original function, so it just returns 2 basic streams.
;(defun cl-gitinterface::run-base-sh ()
;  (multiple-value-bind (in out) (sb-posix:pipe)
;    (let ((input (sb-sys:make-fd-stream in
;					:input t
;					:external-format :ascii
;					:buffering :none :name "in"))
;	  (output (sb-sys:make-fd-stream out
;					 :output t
;					 :external-format :ascii
;					 :buffering :none :name "out")))
;      (values output input))))


;;;;;;;;; TESTS ;;;;;;;;;;


;Define the failure messages
(macrolet ((def-build-base-exception-checks ()
	     `(progn
		,@(loop for k in (list 
				    :pull
				    :push
				    :commit
				    :remote-add
				    :clone
				    :status) collect
		       `(define-test ,(format nil "test-git-~A-condition-~A" (symbol-name k) "failure")
			  (assert-error (intern ,(format nil "GIT-~A-ERROR" (symbol-name k)) :cl-gitinterface)
					(error (intern ,(format nil "GIT-~A-ERROR" (symbol-name k)) :cl-gitinterface))))))
	     ))
  (def-build-base-exception-checks))

;Tests to define a command, and ensure that the record is in the correct area.
(define-test "define-git-test-command" 
  (let ((testcmd :test))
    (assert-true (cl-gitinterface::define-git-command testcmd))
    (assert-eql testcmd (first cl-gitinterface::*git-commands*))
    (assert-error (intern (format nil "GIT-~A-ERROR" (symbol-name testcmd)) :cl-gitinterface) (error (intern (format nil "GIT-~A-ERROR" (symbol-name testcmd)) :cl-gitinterface)))))

(define-test "test-probe-stream" 
  (let ((x (make-string-input-stream "test-probe-stream succeed"))
	(y (make-string-input-stream "")))
    (assert-eql t (cl-gitinterface::probe-stream x))
    (assert-error (intern "BREAK-LIMIT-REACHED" :cl-gitinterface)
		  (cl-gitinterface::probe-stream y 1))))