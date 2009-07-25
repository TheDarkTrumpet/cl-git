#|
cl-git - asd definition file for git interactions
ln -s /home/.../asd_file /usr/local/lib/sbcl/site-systems (Ubuntu)-- will be needed for this to work.
                         /usr/lib/sbcl/site-systems (Gentoo)
Easy Install:
|#

(common-lisp:defpackage #:cl-git-system
  (:use #:common-lisp #:asdf))

(common-lisp:in-package #:cl-git-system)

;;; MAIN APPLICATON DEFINITION
(defsystem :cl-git
  :description "Common Lisp library that supports interactions with GIT"
  :version "0.1-PRE"
  :author "David Thole <dthole@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:cl-ppcre :local-time)
  :components ((:file "defpackage")
               (:file "cl-git"
                      :depends-on ("defpackage"))))

;;; UNIT TESTS DEFINITON
(defsystem :cl-git-test
  :depends-on (:cl-git :lisp-unit)
  :components ((:module "test"
			:serial t
			:components ((:file "cl-git-test")))))

(defmethod perform ((o test-op) (c (eql (find-system :cl-git))))
  (operate 'load-op :cl-git-test)
  (funcall (intern (symbol-name :run-all-tests) (find-package :cl-git-test))))