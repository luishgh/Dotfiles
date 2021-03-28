;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Luis Henrique Gomes Higino"
      user-mail-address "luishenriquegh2701@gmail.com")

(setq doom-font (font-spec :family "JetBrains Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-dracula) ;; << This line enables the doom-dracula theme

(after! org
  (setq org-directory "~/Documents/org/")
  (setq org-agenda-files '("~/Documents/org/agenda.org"))
  (setq org-log-done 'time)
  )

(defun dt/org-babel-tangle-async (file)
  "Invoke `org-babel-tangle-file' asynchronously."
  (message "Tangling %s..." (buffer-file-name))
  (async-start
   (let ((args (list file)))
     `(lambda ()
        (require 'org)
        ;;(load "~/.emacs.d/init.el")
        (let ((start-time (current-time)))
          (apply #'org-babel-tangle-file ',args)
          (format "%.2f" (float-time (time-since start-time))))))
   (let ((message-string (format "Tangling %S completed after " file)))
     `(lambda (tangle-time)
        (message (concat ,message-string
                         (format "%s seconds" tangle-time)))))))

(defun dt/org-babel-tangle-current-buffer-async ()
  "Tangle current buffer asynchronously."
  (dt/org-babel-tangle-async (buffer-file-name)))

(setq display-line-numbers-type 'relative)

(defun typescript-mode-setup ()
  "Custom setup for Typescript mode"
  (setq flycheck-checker 'javascript-eslint)
  )
(add-hook 'typescript-mode-hook 'typescript-mode-setup)

;;(defun arduino-mode-setup()
;;  (setq flycheck-checker 'platformio-mode)
;;  )
;;(add-hook 'arduino-mode-hook 'arduino-mode-setup)

(setq flutter-sdk-path "/home/luishgh/flutter")
