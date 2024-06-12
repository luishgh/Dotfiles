(define-module (guix-home services shells)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)

  #:use-module (guix-home utils)

  #:export (home-bash-service))

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

                            "export PS1")))))

(define home-bash-service
  (service home-bash-service-type bash-configuration))
