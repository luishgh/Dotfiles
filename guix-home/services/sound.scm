(define-module (guix-home services sound)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:export (home-sound-service-type))

(define (home-sound-profile-service config)
  (list pulseaudio
        pulsemixer))

(define (home-sound-shepherd-services config)
  (list
   ;; Start Pipewire daemon
   (shepherd-service
    (provision '(pulseaudio))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pulseaudio "/bin/pulseaudio")))))))

(define home-sound-service-type
  (service-type (name 'home-sound)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-sound-profile-service)
                       (service-extension
                        home-shepherd-service-type
                        home-sound-shepherd-services)))
                (default-value #f)))
