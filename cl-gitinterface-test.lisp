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
(defun cl-gitinterface::get-from-shell (stream)
  (declare (ignore stream))
  (format t "in new git-from-shell: ~a~%" *cmd-cur*)
  (let ((retvar ""))
    (cond
      ((eql *cmd-cur* "pull")
       (setf retvar "repository clean!")))
    retvar))

;Stub out the run-base-sh from the original function, so it just returns 2 basic streams.
(defun cl-gitinterface::run-base-sh ()
  (multiple-value-bind (in out) (sb-posix:pipe)
    (let ((input (sb-sys:make-fd-stream in
					:input t
					:external-format :ascii
					:buffering :none :name "in"))
	  (output (sb-sys:make-fd-stream out
					 :output t
					 :external-format :ascii
					 :buffering :none :name "out")))
      (values output input))))


;;;;;;;;; TESTS ;;;;;;;;;;

;Define the failure messages
(macrolet ((def-test-fail ()
	     `(progn
		,@(loop for k in '(:pull :push :commit :remote-add :clone :status) collect
		       `(define-test ,(concatenate 'string "test-git-" (symbol-name k) "-failure")
			  (assert-error (intern ,(string-upcase (concatenate 'string "git-" (string-downcase (symbol-name k)) "-error")) :CL-GITINTERFACE)
			  (cl-gitinterface::verify-git-cmd "Automatic merge failed....error: failed to push some refs fatal: cannot do a partial commit during a merge fatal: remote origin already exists fatal: kljlkj" ,k)))))))
  (def-test-fail))

;(define-test test-git-pull-failure
;  (assert-error 'CL-GITINTERFACE::git-pull-error (cl-gitinterface::verify-git-cmd "Automatic merge failed; fix this now!" :pull)))