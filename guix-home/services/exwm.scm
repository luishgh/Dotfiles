(define-module (guix-home services exwm)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages compton))

(define (home-exwm-profile-service config)
  (list polybar
        picom))

(define (home-exwm-files-service config)
  (list (list ".xsession"
              (program-file ".xsession"
                            #~(system
                               #$(canonicalize-path (string-append
                                                     (getcwd)
                                                     "../.emacs.d/exwm/start-exwm.sh")))))))

(define home-exwm-service-type
  (service-type (name 'home-exwm)
                (description "Start EXWM in .xsession and install its dependencies.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-exwm-profile-service)
                       (service-extension
                        home-files-service-type
                        home-exwm-files-service)))
                (default-value #f)))
