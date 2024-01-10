(define-module (guix-home services gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix-home utils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home services gnupg)
  #:export (home-gnupg-service))

(define home-gnupg-service
  (service home-gpg-agent-service-type
                 (home-gpg-agent-configuration
                  (pinentry-program
                   (file-append pinentry-emacs "/bin/pinentry-emacs"))
                  (ssh-support? #t)
                  (extra-content "
allow-emacs-pinentry
allow-loopback-pinentry
"))))
