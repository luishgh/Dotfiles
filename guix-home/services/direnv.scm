(define-module (guix-home services direnv)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages shellutils)
  #:export (home-direnv-service-type))

(define (home-direnv-profile-service config)
  (list direnv))

(define (home-direnv-bash-service config)
  (home-bash-extension
   (bashrc
    (list (plain-file "direnv-bash-hook.sh" "eval \"$(direnv hook bash)\"")))))

(define (home-direnv-files-service config)
  (list (list ".config/direnv/direnvrc"
              (local-file "../files/direnvrc"))))

(define home-direnv-service-type
  (service-type (name 'home-direnv)
                (description "Install and configure direnv.")
                (extensions
                 (list (service-extension
                        home-bash-service-type
                        home-direnv-bash-service)
                       (service-extension
                        home-files-service-type
                        home-direnv-files-service)
                       (service-extension
                        home-profile-service-type
                        home-direnv-profile-service)))
                (default-value #f)))
