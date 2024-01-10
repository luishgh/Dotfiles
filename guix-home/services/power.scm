(define-module (guix-home services power)
  #:use-module (gnu services)
  #:use-module (gnu home services pm)
  #:export (home-power-service))

(define home-power-service
  (service home-batsignal-service-type
                 (home-batsignal-configuration
                  (warning-level 50)
                  (critical-level 40)
                  (danger-level 30))))
