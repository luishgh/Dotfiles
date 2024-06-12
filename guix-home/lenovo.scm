(define-module (guix-home home)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services syncthing)
  #:use-module (guix-home services shells)
  #:use-module (guix-home services desktop)
  #:use-module (guix-home services wayland)
  #:use-module (guix-home services dwl)
  #:use-module (guix-home services sound)
  #:use-module (guix-home services flatpak)
  #:use-module (guix-home services emacs)
  #:use-module (guix-home services direnv)
  #:use-module (guix-home services power)
  #:use-module (guix-home services gnupg))

(home-environment
 (services
  (append (list home-bash-service

                ;; Set up desktop environment
                (service home-desktop-service-type)
                (service home-wayland-service-type)
                home-power-service
                ;;home-gnupg-service
                (service home-syncthing-service-type)

                (service home-flatpak-service-type)
                (service home-direnv-service-type))
          home-sound-services
          home-dwl-services)))
