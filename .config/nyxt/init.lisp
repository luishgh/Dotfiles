(in-package #:nyxt-user)

(dolist (file (list
	       (nyxt-init-file "style.lisp")
	       (nyxt-init-file "keybindings.lisp")))
  (load file))

(define-configuration browser
  ((external-editor-program "emacsclient")))
