;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun lhgh/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'lhgh/display-startup-time)

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
  ;; :straight (if lhgh/is-guix-system
  ;;               nil
  ;;             '(corfu :host github
  ;;                     :repo "minad/corfu"))
  :straight nil
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

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

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
  :font "JetBrains Mono 11"
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Iosevka Aile 15"
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
  (unless (or lhgh/is-guix-system
              (find-font (font-spec :name "all-the-icons")))
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t) ; sets the proper theme

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

(lhgh/leader-maps
  "o" '(:ignore t :which-key "org"))

(defun lhgh/org-mode-setup ()
  (org-indent-mode)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . lhgh/org-mode-setup)
  :commands (org-capture org-agenda) ;; Org is deferred, these commands are autoloaded so they can be used before opening an Org file
  :general
  (lhgh/leader-maps org-mode-map
    "mh" '(consult-org-heading :which-key "find-header")
    "mtc" '(org-toggle-checkbox :which-key "checkbox"))
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
[DEFAULT-PACKAGES]
\\usepackage[]{babel}
\\pagenumbering{gobble}
\\usepackage[margin=0.5in]{geometry}
\\usepackage{enumitem}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; This ends the use-package org block
)

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  (org-superstar-remove-leading-stars t))

(use-package grip-mode
  :straight t
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
                      (expand-file-name "~/.emacs.d/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'lhgh/org-babel-tangle-config)))

(lhgh/leader-maps
  "v" '(:ignore t :which-key "previews")
  "m" '(:ignore t :which-key "mode")
  "mt" '(:ignore t :which-key "toggle")
  "r" '(:ignore t :which-key "generate"))

(use-package projectile
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :general
  (lhgh/leader-maps
    "p"  '(:ignore t :which-key "projectile")
    "pp" '(projectile-switch-project :which-key "switch-project")
    "pf" '(projectile-find-file :which-key "find-file")
    "pF" '(projectile-find-file-other-window :which-key "find-file-other-window")
    "pq" '(projectile-kill-buffers :which-key "quit project")
    "pt" '(projectile-test-project :which-key "test-project"))
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

(use-package magit-todos ;; shows TODOs (or similars) in files inside the repo 
  :defer t)

(use-package forge
  :init
  (setq auth-sources '("~/.authinfo"))
  :after magit)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-eldoc-enable-hover nil)
  ;; (lsp-ui-doc-position 'bottom))
  (lsp-lens-enable nil)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-signature-render-documentation nil)
  (lsp-ui-doc-show-with-cursor nil))

(push "~/.local/bin" exec-path)

(use-package dap-mode
  :straight t
  :after lsp-mode
  :custom
  (lsp-emable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  :general
  (lhgh/leader-maps
    "d"  '(:ignore t :which-key "debugger")
    "dd" '(dap-debug "debug")
    "dl" '(dap-debug-last :which-key "debug-last")
    "dr" '(dap-debug-recent :which-key "debug-recent")
    "du" '(:ignore t :which-key "ui")
    "dur" '(dap-ui-repl :which-key "repl")
    "dul" '(dap-ui-locals :which-key "locals")
    "dub" '(dap-ui-breakpoints :which-key "breakpoints")
    "due" '(dap-ui-expressions :which-key "expresions")
    "dh" '(dap-hydra :which-key "dap-hydra")
    "db" '(:ignore t :which-key "breakpoints")
    "dbt" '(dab-breakpoint-toggle :which-key "toggle")
    "dbl" '(dap-breakpoint-log-message :which-key "log-message")
    "dbc" '(dap-breakpoint-condition :which-key "condition")
    "ds" '(dap-switch-stack-frame :which-key "stack-frame") 
    "dq" '(dap-disconnect :which-key "disconnect")
    "de" '(dap-debug-edit-template :which-key "edit-template")))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)

  ;; debugger
  (require 'dap-node)
  (dap-node-setup)) ;; Automatically installs Node debug adapter if needed

(use-package python-mode
  ;; :hook (python-mode . lsp-deferred)
  :straight (:type built-in)
  :general
  (lhgh/leader-maps python-mode-map
    "mr"  '(:ignore t :which-key "repl")
    "mrr" '(run-python :which-key "run repl")
    "mre" '(python-shell-send-region :which-key "send region to repl")
    "mrE" '(python-shell-send-buffer :which-key "send buffer to repl")
    "mrf" '(python-shell-send-file :which-key "send function to repl")
    "mrF" '(python-shell-send-defun :which-key "send file to repl"))
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package dart-mode
  :mode "\\.dart\\'")

(use-package lsp-dart
  :straight t
  :hook (dart-mode . lsp-deferred)
  :custom
  (lsp-dart-flutter-sdk-dir (if lhgh/is-guix-system
                                (string-trim (shell-command-to-string "find /nix/store -regex \".*flutter\-.*\-unwrapped$\""))
                              "~/.local/share/flutter"))
  (lsp-dart-sdk-dir (concat lsp-dart-flutter-sdk-dir "/bin/cache/dart-sdk")))

(use-package flutter
  :straight t
  :after dart-mode
  :general
  (lhgh/leader-maps dart-mode-map
    "mr" '(flutter-run-or-hot-reload :which-key "hot reload")))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq makrdown-command "marked"))

(use-package markdown-toc
  :straight t
  :after markdown-mode
  :general
  (lhgh/leader-maps '(markdown-mode-map gfm-mode-map)
    "rt" '(markdown-toc-generate-or-refresh-toc :which-key "generate or refresh markdown toc")))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :hook (web-mode . lsp-deferred)
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package lsp-tailwindcss
  :straight '(:type git
              :host github
              :repo "merrickluo/lsp-tailwindcss"))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

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
  :straight t
  :after eshell)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))


(use-package eshell
  :hook (eshell-first-time-mode . lhgh/configure-eshell)
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
    :straight t
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
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega uses it
               "vimiv"
               '(file)))))

(use-package telega
  ;; :straight (telega :host github
  ;;                   :branch "master")
  :commands telega
  :custom
  (telega-completing-read-function 'completing-read)
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
  :custom
  (elcord-display-buffer-details nil)
  :config
  (elcord-mode))

(use-package elpher
  :commands elpher)

(use-package langtool
  :straight t
  :commands langtool-check
  :config
  (setq langtool-language-tool-server-jar "~/.local/bin/LanguageTool-5.3/languagetool-server.jar"))

(require 'lhgh-mail)

(use-package pinentry
  :straight (:source gnu-elpa-mirror)
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
 (setq gc-cons-threshold (* 2 1000 1000))
