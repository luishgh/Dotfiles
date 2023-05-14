(define-module (guix-home services sound)
  #:use-module (gnu services)
  #:use-module (gnu home services desktop)
  #:use-module (guix-home services pipewire)
  #:export (home-sound-services))

(define home-sound-services
  (list (service home-dbus-service-type)
        (service home-pipewire-service-type)))
