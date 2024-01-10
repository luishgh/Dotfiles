(define-module (guix-home services wayland)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages chromium)

  #:use-module (nongnu packages mozilla)

  #:export (home-wayland-service-type))

(define (home-wayland-environment-variables-service _)
  '(("xdg_session_type" . "wayland")
    ("moz_enable_wayland" . "1")))

(define (home-wayland-profile-service config)
  (list
   grim
   slurp
   wl-clipboard
   xsel

   ;; Browsers (those that have different versions for Xorg and Wayland)
   ungoogled-chromium/wayland))

(define home-wayland-service-type
  (service-type (name 'home-wayland)
                (description "Configure a Wayland desktop.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-wayland-profile-service)
                       (service-extension
                        home-environment-variables-service-type
                        home-wayland-environment-variables-service)))
                (default-value #f)))
