; To run:
; (require :cl-gitinterface)
; (in-package :cl-gitinterface)
; (load "/this/path/cl-gitinterface-test.lisp")
; (run-tests)

(eval-when (:compile-toplevel :load-toplevel
			      :execute) (require :cl-gitinterface))

(in-package :cl-gitinterface)

(defvar *cmd-cur* "")
(defvar *cmd-mode* "error")

;Stub out the get-from-shell function, since we want to test through the use of errors.
(defun get-from-shell (stream)
  (declare (ignore stream))
  (let ((retvar ""))
    (cond
      ((eql *cmd-cur* "status")
       (setf retvar "repository clean!")))
    retvar))

;Stub out the run-base-sh from the original function, so it just returns 2 basic streams.
(defun run-base-sh ()
  (let* ((out (make-string-output-stream))
	 (in (make-concatenated-stream out)))
    (values in out)))