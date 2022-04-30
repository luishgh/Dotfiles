(define-module (guix-home services desktop)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages image)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages video)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages virtualization)
  #:use-module (nongnu packages compression)
  #:export (home-desktop-service-type))

(define (home-desktop-files-service config)
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

(define (home-desktop-profile-service config)
  (list stumpwm+slynk
        `(,stumpwm "lib")
        stumpish
        emacs-stumpwm-mode
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-ttf-fonts
        sbcl-stumpwm-stumptray
        sbcl-stumpwm-kbd-layouts
        sbcl

        ;; Xorg
        setxkbmap
        picom
        xsettingsd
        flameshot

        ;; Wayland
        grim
        slurp
        wl-clipboard
        xsel

        ;; Utilities
        ncurses ;; for prompt colors
        zip unzip ;; urar
        neofetch
        stow
        openssh
        alacritty
        flatpak

        ;; Browsers
        ungoogled-chromium
        icecat
        qutebrowser
        nyxt

        ;; DE Suite
        libreoffice
        calibre
        zathura
        xournalpp
        mpv

        ;; Credentials management
        password-store
        pinentry
        gnupg

        ;; Appearance
        matcha-theme
        papirus-icon-theme
        adwaita-icon-theme

        ;; Fonts
        font-google-noto
        font-iosevka-aile
        font-jetbrains-mono

        ;; Virtualization
        virt-manager))

(define (home-desktop-shepherd-services config)
  (list
   (shepherd-service
    (provision '(gpg-agent))
    (documentation "Run and control gpg-agent.")
    (start #~(make-system-constructor "gpg-connect-agent /bye"))
    (stop #~(make-system-destructor "gpgconf --kill gpg-agent")))
   (shepherd-service
    (provision '(xsettingsd))
    (documentation "Run the xsettingsd daemon.")
    (respawn? #t)
    (start #~(make-forkexec-constructor
              (list #$ (file-append xsettingsd "/bin/xsettingsd"))))
    (stop #~(make-kill-destructor)))))

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-desktop-profile-service)
                       (service-extension
                        home-shepherd-service-type
                        home-desktop-shepherd-services)
                       (service-extension
                        home-files-service-type
                        home-desktop-files-service)))
                (default-value #f)))
