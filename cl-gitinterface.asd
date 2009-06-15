; ln -s /home/.../asd_file /usr/local/lib/sbcl/site-systems (Ubuntu)-- will be needed for this to work.
;                          /usr/lib/sbcl/site-systems (Gentoo)

(in-package #:cl-user)

(asdf:defsystem :git-interface
  :components ((:file "git-interface"))
  :depends-on (:cl-ppcre))