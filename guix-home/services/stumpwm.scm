(define-module (guix-home services stumpwm)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages wm)
  #:export (home-stumpwm-service))

(define (home-stumpwm-profile-service config)
  (list stumpwm+slynk
        `(,stumpwm "lib")
        stumpish
        emacs-stumpwm-mode
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-ttf-fonts
        sbcl-stumpwm-stumptray
        sbcl-stumpwm-kbd-layouts
        sbcl))

(define home-stumpwm-service
  (simple-service
   'home-stumpwm
   home-profile-service-type
   home-stumpwm-profile-service))
