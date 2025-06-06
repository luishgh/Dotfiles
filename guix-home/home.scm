;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (guix-home home)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services syncthing)
  #:use-module (guix-home services shells)
  #:use-module (guix-home services desktop)
  #:use-module (guix-home services wayland)
  #:use-module (guix-home services sway)
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
                (service home-sway-service-type)
                (service home-emacs-service-type)
                home-power-service
                ;;home-gnupg-service
		(service home-syncthing-service-type)

                (service home-flatpak-service-type)
                (service home-direnv-service-type))
          ;;home-sound-services
          ;%base-home-services
          )))
