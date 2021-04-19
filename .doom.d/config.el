;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Luis Henrique Gomes Higino"
      user-mail-address "luishenriquegh2701@gmail.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 24))

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

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-roam-directory "~/Documents/org/org-roam")

(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-graph-show
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture))

;; use this if you want completion with company
;; (push 'company-capf  company-backends)

;; use this for as-you-type link completions
;; (setq org-roam-completion-everywhere t)

(use-package deft
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Documents/org/org-roam/"))

(setq display-line-numbers-type 'relative)

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(setq elfeed-feeds (quote
                    (("https://www.reddit.com/r/neovim.rss" reddit vim)
                     ("http://neovim.io/news.xml" news neovim)
                     ("https://www.reddit.com/r/Gentoo.rss" reddit gentoo)
                     ("https://www.reddit.com/r/DoomEmacs.rss" reddit emacs)
                     ("https://www.reddit.com/r/vim.rss" reddit vim)
                     ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                     ("https://www.gentoo.org/feeds/news.xml" news gentoo))))

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
