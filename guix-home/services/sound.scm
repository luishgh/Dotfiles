(define-module (guix-home services sound)
  #:use-module (gnu services)
  #:use-module (guix-home services pipewire)
  #:use-module (guix-home services dbus)
  #:export (home-sound-services))

(define home-sound-services
  (list (service home-dbus-service-type)
        (service home-pipewire-service-type)))
