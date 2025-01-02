(define-module (guix-home services power)
  #:use-module (gnu services)
  #:use-module (gnu home services pm)
  #:export (home-power-service))

(define home-power-service
  (service home-batsignal-service-type
                 (home-batsignal-configuration
                  (warning-level 30)
                  (critical-level 25)
                  (danger-level 20))))
