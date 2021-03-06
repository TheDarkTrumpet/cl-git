#|
cl-gitinterface-text-external

This file runs the unit tests that are for the external exported functions for
cl-gitinterface.  This tests what the end user would have access to, in another
package.  This supplements the cl-gitinterface-test.lisp file, which tests
the internal functionality as if it was all shadow imported.

To run:
(load 'cl-git-test-external')
(in-package :cl-git-test-external)
(run-tests)

|#

(eval-when (:compile-toplevel :load-toplevel
			      :execute) (require :cl-git) (require :sb-posix) (require :lisp-unit))

(defpackage :cl-git-test-external
  (:use :cl :cl-git :lisp-unit))

;(use-package :lisp-unit)
(in-package :cl-git-test-external)

(defvar *cmd-cur* "")
(defvar *cmd-mode* "error")

;Stub out the get-from-shell function, since we want to test through the use of errors.
;(defun cl-git::get-from-shell (stream)
;  (declare (ignore stream))
;  (format t "in new git-from-shell: ~a~%" *cmd-cur*)
;  (let ((retvar ""))
;    (cond
;      ((eql *cmd-cur* "pull")
;       (setf retvar "repository clean!")))
;    retvar))

;Stub out the run-base-sh from the original function, so it just returns 2 basic streams.
;(defun cl-git::run-base-sh ()
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
			  (assert-error (intern ,(format nil "GIT-~A-ERROR" (symbol-name k)) :cl-git)
					(error (intern ,(format nil "GIT-~A-ERROR" (symbol-name k)) :cl-git))))))
	     ))
  (def-build-base-exception-checks))

;Tests to define a command, and ensure that the record is in the correct area.
(define-test "define-git-test-command" 
  (let ((testcmd :test))
    (assert-true (cl-git::define-git-command testcmd))
    (assert-eql testcmd (first cl-git::*git-commands*))
    (assert-error (intern (format nil "GIT-~A-ERROR" (symbol-name testcmd)) :cl-git) (error (intern (format nil "GIT-~A-ERROR" (symbol-name testcmd)) :cl-git)))))

(define-test "test-probe-stream" 
  (let ((x (make-string-input-stream "test-probe-stream succeed"))
	(y (make-string-input-stream "")))
    (assert-eql t (cl-git::probe-stream x))
    (assert-error (intern "BREAK-LIMIT-REACHED" :cl-git)
		  (cl-git::probe-stream y 1))))