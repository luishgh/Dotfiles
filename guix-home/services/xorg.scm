(define-module (guix-home services xorg)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (gnu packages xorg)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)

  #:use-module (nongnu packages mozilla)

  #:export (home-xorg-service-type))

(define (home-xorg-files-service config)
  (list (list ".config/xsettingsd/xsettingsd.conf"
              (plain-file "xsettingsd.conf" "
Net/IconThemeName \"Adwaita\"
Net/ThemeName \"Matcha-dark-azul\"
Gtk/DecorationLayout \"menu:minimize,maximize,close\"
Gtk/FontName \"Iosevka Aile 11\"
Gtk/MonospaceFontName \"JetBrains Mono 10\"
Gtk/CursorThemeName \"Adwaita\"
Xft/Antialias 1
Xft/Hinting 0
Xft/HintStyle \"hintnone\" "))))

(define (home-xorg-profile-service config)
  (list ;; Xorg
        setxkbmap
        picom
        xsettingsd
        flameshot

        ;; Browsers (firefox is here due to having different packages for Xorg and Wayland
        firefox

        ;; Appearance
        matcha-theme
        papirus-icon-theme
        adwaita-icon-theme))

(define (home-xorg-shepherd-services config)
  (list
   (shepherd-service
    (provision '(xsettingsd))
    (documentation "Run the xsettingsd daemon.")
    (respawn? #t)
    (start #~(make-forkexec-constructor
              (list #$(file-append xsettingsd "/bin/xsettingsd"))))
    (stop #~(make-kill-destructor)))))

(define home-xorg-service-type
  (service-type (name 'home-wayland)
                (description "Configure an Xorg desktop.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-xorg-profile-service)
                       (service-extension
                        home-files-service-type
                        home-xorg-files-service)
                       (service-extension
                        home-shepherd-service-type
                        home-xorg-shepherd-services)))
                (default-value #f)))
