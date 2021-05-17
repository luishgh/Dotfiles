;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(straight-use-package 'use-package) ;; Use straight.el for use-package expressions
(setq straight-use-package-by-default t) ;; Install a package if it isn't installed already
;; (setq use-package-verbose t) ;; Uncomment to bench mark use-package

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(use-package evil
  :init ;; tweak evil's configuration before loading it (as suggested in the package's documentation)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-want-C-u-scroll t) ;; an optional part of `evil-want-integration', I personally like it
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; C-g is trully equal to ESC

  ;; j and k move through visual lines
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1))

(use-package general
  :after evil
  :config
  (general-create-definer lhgh/leader-maps
    :states '(normal insert emacs)
    :prefix "SPC" ;; The prefix in normal state
    :global-prefix "C-SPC") ;; The prefix accessible in any state

  (lhgh/leader-maps
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(consult-theme :which-key "choose theme")
    "tv" '(visual-line-mode :which-hey "visual lines")))

(use-package hydra
  :defer t) ;; I don't use hydra at the moment but paln to do it when I find a case where it is useful

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-l" . vertico-exit-input)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init 
  (vertico-mode))

(defun lhgh/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :bind (("C-s" . consult-line))
  :custom
  (consult-project-root-function #'lhgh/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package corfu
  :straight '(corfu :host github
                    :repo "minad/corfu")
  :demand t
  :bind (:map corfu-map
         ("M-j" . corfu-next)
         ("M-k" . corfu-previous))
  :custom
  (corfu-cycle t)
  :config
  (setq tab-always-indent 'complete)
  (corfu-global-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(lhgh/leader-maps
  "."  '(find-file :which-key "find file")
  "b" '(:ignore t :which-key "buffers")
  "bb" '(consult-buffer :which-key "switch buffer"))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil
  :font "Fira Code 11"
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu 15"
  :weight 'medium)

(column-number-mode) ;; Shows column number in mode-line
(global-display-line-numbers-mode t) ;; Shows line numbers
(setq display-line-numbers-type 'relative) ;; Relative line numbers

(dolist (mode '(term-mode-hook
                vterm-mode-hook
                dired-mode-hook
                calendar-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (mode '(org-mode-hook
                markdown-mode-hook))
  (add-hook mode (lambda () (visual-line-mode t))))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  ;; Whethe to display the buffer encoding.
  (setq doom-modeline-buffer-encoding nil))

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "May I save your soul?")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 10)
                          (agenda . 5)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

(lhgh/leader-maps
  "o" '(:ignore t :which-key "org"))

(defun lhgh/org-mode-setup ()
  (org-indent-mode)
  (setq evil-auto-indent nil))

(use-package org
  :straight (:type built-in) ;; using the latest version doesn't work at the moment, will resolve it later (related to the built in version being loaded before this one)
  :hook (org-mode . lhgh/org-mode-setup)
  :commands (org-capture org-agenda) ;; Org is deferred, these commands are autoloaded so they can be used before opening an Org file
  :general
  (lhgh/leader-maps org-mode-map
    "mh" '(consult-org-heading :which-key "find-header"))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)

;; NOTE: Subsequent sections are still part of this use-package block!

(setq org-tag-alist
  '((:startgroup) ;; mutually exclusive tags go here

    ;; CEFET tags
    (:startgroup)
    ("@CEFET"     . ?C)
    (:grouptags)
    (:startgroup)
    ("Ensino Médio" . ?E)
    (:grouptags)
    ("Português" . ?p)
    ("Matemática" . ?m)
    ("Física"     . ?f)
    ("História"   . ?h)
    ("Inglês"     . ?i)
    ("Química"    . ?q)
    ("Redação"    . ?r)
    ("Sociologia" . ?s)
    (:endgroup)
    (:startgroup)
    ("Técnico" . ?T)
    (:grouptags)
    ("PDM"        . ?d)
    ("TCC"        . ?t)
    ("PS"         . ?a)
    ("RC"         . ?c)
    ("SO"         . ?o)
    ("TEI"        . ?e)
    (:endgroup)

    ;; Other major tags
    ("@mandarim" . ?M)
    ("@redacao"  . ?R)
    ("@pessoal"  . ?P)
    (:endgroup)))

(setq org-capture-templates
  '(("t" "Tasks")
    ("tt" "Task" entry (file+olp "~/Documents/Org/Agenda/Tasks.org" "Inbox")
      "* TODO %?\n%U\n%a\n%i" :empty-lines 1)))
(lhgh/leader-maps
  "oc" '(org-capture :which-key "capture"))

(setq org-agenda-files
  '("~/Documents/Org/Agenda/Tasks.org"
    "~/Documents/Org/Habits.org"))
(setq org-log-done 'time)

(lhgh/leader-maps
  "oa" '(org-agenda :which-key "agenda"))

;; Custom agenda views
(setq org-agenda-custom-commands
  '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (tags-todo "+@CEFET"
        ((org-agenda-overriding-header "Next CEFET Tasks")
         (org-agenda-max-todos 5)))
      (tags-todo "+@mandarim|@redacao|@pessoal"
        ((org-agenda-overriding-header "Next Other Tasks")))))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-modules 'org-tempo)

  ;; Custom templates for specific languages
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;; This ends the use-package org block
)

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t))

(use-package grip-mode
  :defer t
  :general
  (lhgh/leader-maps '(markdown-mode-map gfm-mode-map org-mode-map)
    "vg" '(grip-mode :which-key "toggle grip preview"))
  :config
  ;; When nil, update the preview after file saves only, instead of also
  ;; after every text change
  (setq grip-update-after-change nil))

(lhgh/leader-maps
  "v" '(:ignore t :which-key "previews")
  "m" '(:ignore t :which-key "mode")
  "r" '(:ignore t :which-key "generate"))

(use-package projectile
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :general
  (lhgh/leader-maps
    "p"  '(:ignore t :which-key "projectile")
    "pp" 'projectile-switch-project)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :general
  (lhgh/leader-maps
    "g"  '(:ignore t :which-key "git")
    "gg" 'magit-status))

(use-package forge
  :init
  (setq auth-sources '("~/.authinfo"))
  :after magit)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(push "~/.local/bin" exec-path)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq makrdown-command "marked"))

(use-package markdown-toc
  :after markdown-mode
  :general
  (lhgh/leader-maps '(markdown-mode-map gfm-mode-map)
    "rt" '(markdown-toc-generate-or-refresh-toc :which-key "generate or refresh markdown toc")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(lhgh/leader-maps
  "a" '(:ignore t :which-key "applications"))

(use-package vterm
  :commands vterm
  :general
  (lhgh/leader-maps
    "at" '(vterm :which-key "vterm"))
  :config
  (setq vterm-max-scrollback 10000))

(defun lhgh/configure-eshell ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (evil-define-key '(normal emacs insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal emacs insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignore-dups t
        eshell-scroll-to-bottom-on-input t
        eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias")))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . lhgh/configure-eshell)
  :config

  (with-eval-after-load 'em-term
    (setq eshell-destroy-buffer-when-process-dies t)
    (dolist (program '( "nmtui"
                        "nvim"))
      (add-to-list 'eshell-visual-commands program)))

  (eshell-git-prompt-use-theme 'powerline))

(use-package all-the-icons-dired
  :defer t)

(use-package dired
  :straight (:type built-in)
  :defer t
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :general
  (lhgh/leader-maps
    "ad" '(dired-jump :which-key "dired-jump"))
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (dired-omit-files "^\\.[^.].*"))
  :config
  (setq delete-by-moving-to-trash t)

  (use-package dired-rainbow
    :after dired
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (add-hook 'dired-mode-hook
            (lambda ()
              ;; (interactive)
              (dired-collapse-mode 1)
              (all-the-icons-dired-mode 1)))

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(use-package openwith
  :after dired
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega uses it
               "vimiv"
               '(file)))))

;; Make gc pauses faster by decreasing the threshold.
 (setq gc-cons-threshold (* 2 1000 1000))