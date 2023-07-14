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

(straight-use-package 'f)
(setq lhgh/is-guix-system (and (require 'f)
                               (string-equal (f-read "/etc/issue")
                                             "\nThis is the GNU system.  Welcome.\n")))

(straight-use-package 'use-package) ;; Use straight.el for use-package expressions
(setq straight-use-package-by-default (not lhgh/is-guix-system)) ;; Install a package if it isn't installed already on non-Guix systems
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

;; Keep backup files under `user-emacs-directory'
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; Keep auto-save files under `user-emacs-directory'
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(push "~/.emacs.d/lisp" load-path)

(setq lhgh/exwm-enabled (and (eq window-system 'x)
                          (seq-contains command-line-args "--use-exwm")))

(when lhgh/exwm-enabled
  (require 'lhgh-desktop))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :init ;; tweak evil's configuration before loading it (as suggested in the package's documentation)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-want-C-u-scroll t) ;; an optional part of `evil-want-integration', I personally like it
  (setq evil-want-Y-yank-to-eol t) ;; Y => y$ (like in Neovim)
  (setq evil-respect-visual-line-mode t) ;; move by visual lines
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)) ;; C-g is trully equal to ESC

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :config
  ;; temporary fix for https://github.com/emacs-evil/evil-collection/pull/720 :/
  (delete 'mu4e evil-collection-mode-list)
  (delete 'mu4e-conversation evil-collection-mode-list)
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config (evilnc-default-hotkeys t) ;; use default key bindings (M-;) in Emacs state
  :bind (:map evil-normal-state-map
         ("gc" . evilnc-comment-or-uncomment-lines)))

(use-package general
  :after evil
  :config
  (general-create-definer lhgh/leader-maps
    :states '(normal insert emacs)
    :prefix "SPC" ;; The prefix in normal state
    :global-prefix "C-SPC") ;; The prefix accessible in any state

  (general-create-definer lhgh/ctrl-c-binds
    :states '(normal insert emacs)
    :prefix "C-c")

  (lhgh/leader-maps
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(consult-theme :which-key "choose theme")
    "tv" '(visual-line-mode :which-hey "visual lines")))

(use-package hydra
  :defer 1) ;; load only when a defhydra is called

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(defun lhgh/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
    ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
    (if (string-match-p "/." (minibuffer-contents))
        (zap-up-to-char (- arg) ?/)
      (delete-minibuffer-contents))
    (backward-delete-char arg)))

(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-l" . vertico-exit-input)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word)
         ("<backspace>" . lhgh/minibuffer-backward-kill))
  ;; :custom-face
  ;; (vertico-current ((t (:background "#3a3f5a"))))
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
  :demand t
  :bind (:map corfu-map
         ("M-j" . corfu-next)
         ("M-k" . corfu-previous)
         ("M-g" . corfu-quit))
  :custom
  (corfu-cycle t)
  :config
  (setq tab-always-indent 'complete)
  (global-corfu-mode 1))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :straight t
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config
  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(lhgh/leader-maps
  "."  '(find-file :which-key "find file")
  "b" '(:ignore t :which-key "buffers")
  "bb" '(consult-buffer :which-key "switch buffer"))

(defun lhgh/set-font-faces ()
  (set-face-attribute 'default nil
    :font "JetBrains Mono 11"
    :weight 'medium)
  (set-face-attribute 'fixed-pitch nil
    :font "JetBrains Mono 11"
    :weight 'medium)
  (set-face-attribute 'variable-pitch nil
    :font "Iosevka Aile 15"
    :weight 'medium))

(if (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
             (with-selected-frame frame
              (lhgh/set-font-faces))))
  (lhgh/set-font-faces))

(column-number-mode) ;; Shows column number in mode-line
;; (global-display-line-numbers-mode t) ;; Shows line numbers globally
(setq display-line-numbers-type 'relative) ;; Relative line numbers

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (mode '(org-mode-hook
                markdown-mode-hook))
  (add-hook mode (lambda () (visual-line-mode t))))

(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (or lhgh/is-guix-system
              (find-font (font-spec :name "all-the-icons")))
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-palenight t) ; sets the proper theme
  (load-theme 'modus-operandi)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-encoding nil))

(use-package dashboard
  :disabled t ; depends on page-break-lines, which is currently breaking Org-roam
  :defer lhgh/exwm-enabled ;; defer if in EXWM because it doesn't make sense in that context
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "May I save your soul?")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 10)
                          (agenda . 5)
                          (projects . 5)))
  (unless lhgh/exwm-enabled
    (dashboard-setup-startup-hook)))

(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(lhgh/leader-maps
  "o" '(:ignore t :which-key "org"))

(defun lhgh/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode)
  (face-remap-add-relative 'tree-sitter-hl-face:punctuation nil  :inherit 'fixed-pitch)
  (setq evil-auto-indent nil))

(when lhgh/is-guix-system
  ;; Use org provided by Guix
  (straight-use-package '(org :type built-in)))

(use-package org
  :hook (org-mode . lhgh/org-mode-setup)
  :commands (org-capture org-agenda) ;; Org is deferred, these commands are autoloaded so they can be used before opening an Org file
  :general
  (org-mode-map
   :states 'normal
   "<tab>" 'org-cycle)
  (lhgh/leader-maps org-mode-map
    "mh" '(consult-org-heading :which-key "find-header")
    "mtc" '(org-toggle-checkbox :which-key "checkbox"))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)

;; NOTE: Subsequent sections are still part of this use-package block!

;; Increase the size of various headings
(with-eval-after-load 'org-indent
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.3 :inherit 'variable-pitch)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(setq org-tag-alist
  '(("@mandarim" . ?M)
    ("@pessoal" . ?P)
    ("@UFMG" . ?U)))

(setq org-capture-templates
  '(("t" "Tasks")
    ("tt" "Task" entry (file+olp "~/Documents/Org/Agenda/Tasks.org" "Inbox")
      "* TODO %?\n%U\n%a\n%i" :empty-lines 1)
    ("m" "Email")
    ("mr" "Read Later" entry (file+olp "~/Documents/Org/Agenda/Tasks.org" "Email")
      "* TODO Read %:subject from %:from\n%a\n%U\n\n%i" :empty-lines 1 :immediate-finish t)))
(lhgh/leader-maps
  "oc" '(org-capture :which-key "capture"))

(setq org-agenda-files
  '("~/Documents/Org/Agenda/Tasks.org"
    "~/Documents/Org/Agenda/Habits.org"))
(setq org-log-done 'time)

(lhgh/leader-maps
  "oa" '(org-agenda :which-key "agenda"))

;; Custom agenda views
(setq org-agenda-custom-commands
  '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (tags-todo "+@UFMG"
        ((org-agenda-overriding-header "Next UFMG Tasks")
         (org-agenda-max-todos 5)))
      (tags-todo "+@mandarim|@pessoal"
        ((org-agenda-overriding-header "Next Other Tasks")))))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (scheme . t)
     (latex . t))))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-modules 'org-tempo)

  ;; Custom templates for specific languages
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("homework"
                   "\\documentclass[11pt]{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage[]{babel}
\\pagenumbering{gobble}
\\usepackage[margin=0.5in]{geometry}
\\usepackage{enumitem}
\\usepackage{hyperref}

[EXTRA]


"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

;; This ends the use-package org block
)

(use-package org-make-toc
  :defer)

(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Documents/biblio.bib"))
  (citar-library-paths '("~/Documents/Library"))
  (citar-notes-paths '("~/Documents/Org/org-roam/reference/"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-symbols `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
                   (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
                   (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  :bind
  (:map org-mode-map :package org
        ;; optional: org-cite-insert is also bound to C-c C-x @
        ("C-c b" . #'org-cite-insert)))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Org/org-roam")
  (org-roam-capture-templates
   '(("m" "main" plain "%?"
      :if-new (file+head "main/${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :if-new (file+head "reference/${title}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "bibliography reference" plain "%?"
      :if-new (file+head "reference/${citekey}.org"
                         "#+title: ${author} :: ${title}\n")
      :unnarrowed t)))
  (org-roam-node-display-template
   (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :general
  (lhgh/ctrl-c-binds
    "n" '(:ignore t :which-key "notes")
    "nl" 'org-roam-buffer-toggle
    "nf" 'org-roam-node-find
    "ni" 'org-roam-node-insert)
  :config
  (org-roam-db-autosync-enable)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-roam-ref-format 'org-cite)
  (bibtex-completion-bibliography org-cite-global-bibliography)
  (bibtex-completion-notes-path org-roam-directory)
  (bibtex-completion-library-path "~/Documents/Library"))

(use-package anki-editor
  :defer t)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
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

(use-package org-mime
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))

(use-package org-appear
  :after org
  :straight (org-appear
             :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t))

;; Automatically tangle our Emacs.org config file when we save it
(defun lhgh/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.dotfiles/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'lhgh/org-babel-tangle-config)))

(lhgh/leader-maps
  "v" '(:ignore t :which-key "previews")
  "m" '(:ignore t :which-key "mode")
  "mt" '(:ignore t :which-key "toggle")
  "r" '(:ignore t :which-key "generate"))

(use-package project
  :straight (:type built-in)
  :after projectile
  :config
  (customize-set-value 'xref-search-program 'ripgrep))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :general
  (lhgh/leader-maps
    "g"  '(:ignore t :which-key "git")
    "gg" 'magit-status))

(use-package magit-todos ;; shows TODOs (or similars) in files inside the repo
  :after magit)

(use-package forge
  :after magit)

(use-package eglot)

(push "~/.local/bin" exec-path)

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

  ;; Remove yas-expand from tab
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; Bind yas-expand to C-tab
  (define-key yas-minor-mode-map (kbd "C-<tab>") #'yas-expand)
  (yas-reload-all))

(use-package lispy
  ;; :disabled
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  ;; :disabled
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators c-w additional prettify additional-movement text-objects)))

(use-package symex
  :disabled
  :hook ((emacs-lisp-mode . symex-mode)
         (scheme-mode . symex-mode))
  :general
  (symex-mode-map
   "C-;" 'symex-mode-interface)
  :custom
  (symex-modal-backend 'evil)
  :config
  (symex-initialize))

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(use-package geiser
  :hook (scheme-mode . geiser-mode))
(use-package geiser-guile
  :defer t)

(use-package sly
  :mode "\\.lisp\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indent-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . flymake-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :straight (:type built-in)
  :general
  (lhgh/leader-maps python-mode-map
    "mr"  '(:ignore t :which-key "repl")
    "mrr" '(run-python :which-key "run repl")
    "mre" '(python-shell-send-region :which-key "send region to repl")
    "mrE" '(python-shell-send-buffer :which-key "send buffer to repl")
    "mrf" '(python-shell-send-defun :which-key "send function to repl")
    "mrF" '(python-shell-send-file :which-key "send file to repl"))
  :custom
  (python-shell-interpreter "python3"))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode)
  :straight '(:type git
              :host github
              :repo "glyph/python-docstring-mode"))

(use-package dart-mode
  :mode "\\.dart\\'")

(use-package flutter
  :straight t
  :after dart-mode
  :general
  (lhgh/leader-maps dart-mode-map
    "mr" '(flutter-run-or-hot-reload :which-key "hot reload")))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "marked"))

(use-package markdown-toc
  :straight t
  :after markdown-mode
  :general
  (lhgh/leader-maps '(markdown-mode-map gfm-mode-map)
    "rt" '(markdown-toc-generate-or-refresh-toc :which-key "generate or refresh markdown toc")))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (text-mode . rainbow-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package tree-sitter
  :straight t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter
  :straight t)

(use-package envrc
  :after project
  :config
  (envrc-global-mode))

(use-package compile
 :hook (compilation-filter . ansi-color-compilation-filter)
 :custom (ansi-color-bold-is-bright 't))

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

  (require 'xterm-color)

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (evil-define-key '(normal emacs insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal emacs insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)

  ;; Required for keymaps to work
  (evil-normalize-keymaps)

  (setenv "PAGER" "cat")

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignore-dups t
        eshell-scroll-to-bottom-on-input t
        eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias")))

(use-package eshell-git-prompt
  :straight t
  :after eshell)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))


(use-package eshell
  :hook ((eshell-first-time-mode . lhgh/configure-eshell)
         (eshell-mode . eshell-alias-initialize))
  :config
  (lhgh/leader-maps
    "ae" '(eshell :which-key "eshell"))

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
           (dired-omit-files "^\\.[^.].*")
           (dired-kill-when-opening-new-dired-buffer 't)
           (dired-compress-directory-default-suffix ".zip"))
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
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "H" 'dired-omit-mode
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(use-package openwith
  :after dired
  :hook (dired-mode . openwith-mode)
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
                '("pdf"))
               "zathura"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "gif" "bmp" "tif" "jpeg")) ;; Removed jpg and png because Telega uses them
               "vimiv"
               '(file)))))

(use-package telega
  ;; :straight (telega :host github
  ;;                   :branch "master")
  :commands telega
  :custom
  (telega-completing-read-function 'completing-read)
  (telega-sticker-set-download 't)
  (telega-emoji-use-images nil)
  :config
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (telega-appindicator-mode 1))

(use-package erc
    :commands erc
    :config
    ;; general setup
    (setq erc-server "irc.libera.chat"
          erc-nick "luishgh"
          erc-user-full-name "Luis Henrique"
          erc-kill-buffer-on-part t
          erc-auto-query 'bury
          erc-autojoin-channels-alist '(("libera.chat" "#systemcrafters")))

    ;; visual config
    (setq erc-fill-column 120
          erc-fill-function 'erc-fill-static
          erc-fill-static-center 20)

    ;; friends
    (setq erc-pals '("diegovsky"))

    ;; tracking config
    (setq erc-track-exclude '("#emacs" "#guix")
          erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY")
          erc-track-exclude-server-buffer t
          erc-track-shorten-start 8
          erc-track-visibility nil))

(defun lhgh/connect-irc ()
  (interactive)
  (erc-tls
   :server "irc.libera.chat" :port 7000
   :nick "luishgh" :password (password-store-get "irc/irc.libera.chat")))

(use-package elcord
  :straight t
  :defer t
  :custom
  (elcord-display-buffer-details nil))

(use-package mastodon
  :config
  (setq mastodon-instance-url "https://emacs.ch"
        mastodon-active-user "luishgh"))

(use-package elpher
  :commands elpher)

(when lhgh/is-guix-system
  ;; Load mu4e from mail profile
  (let ((default-directory (expand-file-name "~/.guix-extra-profiles/mail/mail/share/emacs")))
    (message default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(require 'lhgh-mail)

(use-package pomm
  :commands pomm
  )

(defun lhgh/bibtex-get-key (bibtex-string)
  "Get cite key from BIBTEX-STRING."
  (when (stringp bibtex-string)
    (with-temp-buffer
      (bibtex-mode)
      (insert bibtex-string)
      (bibtex-generate-autokey))))

(defun lhgh/biblio--selection-insert-at-org-cite-bibfile-callback (bibtex entry)
  "Add BIBTEX (from ENTRY) to end of first file in `org-cite-global-bibliography'."
  (with-current-buffer (find-file-noselect (car org-cite-global-bibliography))
    (save-excursion
      (bibtex-mode)
      (goto-char (point-max))
      (insert "\n")
      (save-restriction
        (narrow-to-region (point) (point-max))
        (insert bibtex)
        (bibtex-clean-entry)
        (let ((current-key (bibtex-key-in-head))
              (new-key (bibtex-generate-autokey)))
          (when (not (string= current-key new-key))
            (message (format "Inserting autokey %s to replace %s" new-key current-key))
            (goto-char (point-min))
            (search-forward current-key)
            (replace-match new-key))))
      (bibtex-sort-buffer)
      (save-buffer)))
  (message "Inserted bibtex entry for %S."
           (biblio--prepare-title (biblio-alist-get 'title entry))))

(defun lhgh/biblio-selection-insert-at-org-cite-bibfile ()
  "Insert BibTeX of current entry in `org-cite-global-bibliography'."
  (interactive)
  (biblio--selection-forward-bibtex #'lhgh/biblio--selection-insert-at-org-cite-bibfile-callback))

(defun lhgh/biblio-selection-add-to-collection ()
  "Insert current entry at global-bibliography and download paper to library."
  (interactive)
  (lhgh/biblio-selection-insert-at-org-cite-bibfile)
  (biblio--selection-extended-action #'biblio-download--action))

(use-package biblio
  :custom
  (biblio-download-directory "~/Documents/Library/")
  :general
  (biblio-selection-mode-map
   "a" #'lhgh/biblio-selection-add-to-collection)
  :init
  (define-advice biblio-download--action (:filter-args (args) replace-identifier-with-key)
    (let* ((record (car args))
           (key nil))
      (biblio--selection-forward-bibtex (lambda (bibtex _)
                                          (setq key (lhgh/bibtex-get-key bibtex))))
      (setf (alist-get 'identifier record) key)
      (list record))))

(use-package diary
  :straight (:type built-in)
  :custom
  (diary-file "~/Documents/diary"))

(use-package pinentry
  :straight (:source gnu-elpa-mirror)
  :demand
  :config
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package password-store
  :config
  (setq password-store-password-length 15)
  (auth-source-pass-enable)
  :general
  (lhgh/leader-maps
    "ap" '(:ignore t :which-key "pass")
    "app" '(password-store-copy :which-key "copy password")
    "api" '(password-store-insert :which-key "insert password")
    "apg" '(password-store-generate :which-key "generate password")))

(defun lhgh/lookup-password (&rest keys)
  "Gets the password for the query from .authinfo.gpg."
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
        nil)))

;; Make gc pauses faster by decreasing the threshold.
 (setq gc-cons-threshold (* 20 1000 1000))
