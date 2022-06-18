;;; -*- mode: common-lisp; -*-
(in-package #:nyxt-user)

(defvar *my-keymap* (make-keymap "my-map"))

;; Custom bindings
(define-key *my-keymap*
  "y y" 'nyxt/web-mode:copy)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   ;; scheme:cua *my-keymap*
                   ;; scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*
                   ))))

;; Enabling `my-mode' and Vi bindings by default
(define-configuration (buffer web-buffer)
  ((default-modes (append '(nyxt::emacs-mode) '(my-mode) %slot-default%))))
