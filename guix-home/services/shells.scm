(define-module (guix-home services shells)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)

  #:use-module (guix-home utils)

  #:export (home-bash-service))

(define (augment-path-string . paths)
  ;; Generate a valid string to augment PATH with
  ;; all paths passed
  (apply string-append (append
                        (map
                         (lambda (path)
                           (string-append
                            "\""
                            path
                            "\":"))
                         paths)
                        '("\"$PATH\""))))

(define-syntax-rule (augment-path! path ...)
  (cons "PATH" (augment-path-string path ...)))

(define home-environment-variables-alist
  `(;; Many build scripts expect CC to contain the compiler command
    ("CC" . "gcc")

    ;; Make Flatpak apps visible to launcher
    ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
    
    ;; We're in Emacs, yo
    ("VISUAL" . "emacsclient")
    ("EDITOR" . "\"$VISUAL\"")

    ;; Augment PATH
    ,(augment-path!
      "$HOME/.local/bin"
      "$HOME/.yarn/bin"
      "$HOME/.cargo/bin")))

(define bash-configuration
  (home-bash-configuration
   (guix-defaults? #f) ;; prefer to have full control
   (bashrc
    (list (home-config-file "bashrc"
                            ;; Export 'SHELL' to child processes.  Programs such as 'screen'
                            ;; honor it and otherwise use /bin/sh.
                            "export SHELL"

                            ;; Test for an interactive shell.  There is no need to set anything
                            ;; past this point for scp and rcp, and it's important to refrain from
                            ;; outputting anything in those cases.
                            "if [[ $- != *i* ]] ; then"
                                     ;; Shell is non-interactive.  Be done now!
                            "        return"
                            "fi"

                            ;; Source the system-wide file.
                            "source /etc/bashrc"

                            ;; Prompt
                            "if [ -n \"$GUIX_ENVIRONMENT\" ]"
                            "then"
                            ;; Guix env/shell prompt
                            "    PS1=\"\\u@\\h \\W [guix-env]\\$ \""

			    "else"

                            ;; Normal prompt
                            "    PS1=\"\\[$(tput bold)\\]\\[$(tput setaf 1)\\][\\[$(tput setaf 3)\\]\\u\\[$(tput setaf 2)\\]@\\[$(tput setaf 4)\\]\\h \\[$(tput setaf 5)\\]\\W\\[$(tput setaf 1)\\]]\\[$(tput setaf 7)\\]\\\\$ \\[$(tput sgr0)\\]\""
                            "fi"

                            "export PS1")))
   (bash-profile
    (list (home-config-file "bash-profile" ;; Load the default Guix profile
                            "GUIX_PROFILE=\"/home/luishgh/.guix-profile\""
                            ". \"$GUIX_PROFILE/etc/profile\""

                            ;; Load additional Guix profiles
                            "GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles"
                            "for i in \"$GUIX_EXTRA_PROFILES\"/*; do"
                            "  profile=$i/$(basename \"$i\")"
                            "  if [ -f \"$profile\"/etc/profile ]; then"
                            "    GUIX_PROFILE=\"$profile\""
                            "    . \"$GUIX_PROFILE\"/etc/profile"
                            "  fi"
                            "  unset profile"
                            "done"

                            ;; Load Nix environment
                            "if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then"
                            "    . /run/current-system/profile/etc/profile.d/nix.sh"
                            "fi"

                            ;; Don't use the system-wide PulseAudio configuration
                            "unset PULSE_CONFIG"
                            "unset PULSE_CLIENTCONFIG"

                            "# environment variables:")))
   (environment-variables
    home-environment-variables-alist)))

(define home-bash-service
  (service home-bash-service-type bash-configuration))
