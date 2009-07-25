#|
cl-git - asd definition file for git interactions
ln -s /home/.../asd_file /usr/local/lib/sbcl/site-systems (Ubuntu)-- will be needed for this to work.
                         /usr/lib/sbcl/site-systems (Gentoo)
Easy Install:
|#

(common-lisp:defpackage #:cl-git-system
  (:use #:common-lisp #:asdf))

(common-lisp:in-package #:cl-git-system)

(defsystem :cl-git
  :description "Common Lisp library that supports interactions with GIT"
  :version "Draft"
  :author "David Thole <dthole@gmail.com>"
  :license "MIT"
  :components ((:file "defpackage")
               (:file "cl-git"
                      :depends-on (:cl-ppcre :local-time))))
