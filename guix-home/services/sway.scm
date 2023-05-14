(define-module (guix-home services sway)
  #:use-module (guix gexp)

  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:export (home-sway-service-type))

(define (home-sway-files-service config)
  (list (list ".config/sway/config"
              (local-file "../files/sway-config"))
        (list ".config/sway/swaybar-command.sh"
              (program-file "swaybar-command.sh"
                            #~(system
                               #$(canonicalize-path (string-append
                                                     (getcwd)
                                                     "/files/swaybar-command.sh")))))))

(define (home-sway-environment-variables-service _)
  '(("xdg_current_desktop" . "sway")))

(define home-sway-service-type
  (service-type (name 'home-sway)
                (description "Configure sway.")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-sway-files-service)
                       (service-extension
                        home-environment-variables-service-type
                        home-sway-environment-variables-service)
                       (service-extension
                        home-profile-service-type
                        (const (list waybar)))))
                (default-value #f)))
