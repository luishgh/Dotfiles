(define-module (guix-home services desktop)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnome) ;; libnotify
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages video)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages aspell)

  #:use-module (nongnu packages mozilla)

  #:use-module (guix-home utils)

  #:export (home-desktop-service-type))

(define (home-desktop-profile-service config)
  (list ;; Utilities
        xdg-utils
        ncurses ;; for prompt colors
        zip unzip
        neofetch
        stow
        openssh
        alacritty
        foot
        flatpak
        transmission
        curl
        nss-certs

        ;; Browsers
        firefox
        qutebrowser
        nyxt

        ;; DE Suite
        calibre
        zathura zathura-pdf-mupdf
        mpv
        aspell
        aspell-dict-en
        aspell-dict-pt-br

        ;; Notifications
        dunst
        libnotify

        ;; Credentials management
        password-store
        pinentry
        gnupg
        browserpass-native

        ;; Fonts
        font-google-noto
        font-google-noto-emoji
        font-iosevka-aile
        font-jetbrains-mono

        ;; Virtualization
        virt-manager))

(define (home-desktop-xdg-mime-applications-service config)
  (home-xdg-mime-applications-configuration
   (default '((application/x-bittorrent . transmission.desktop)
              (application/x-pdf . org.pwmt.zathura.desktop)
              (application/pdf . org.pwmt.zathura.desktop)
              (video/mp4 . mpv.desktop)
              (x-scheme-handler/magnet torrent.desktop)))
   (desktop-entries
    (list (xdg-desktop-entry
           (file "transmission")
           (name "Bittorent client")
           (type 'application)
           (config
            '((exec . "transmission-remote -a %U"))))))))

(define (home-desktop-files-service config)
  (list (home-config-file "profile"
                          ;; Load the default Guix profile
                          "GUIX_PROFILE=\"/home/luishgh/.guix-profile\""
                          ". \"$GUIX_PROFILE/etc/profile\""

                            ;; Load additional Guix profiles
                            "GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles"
                            "for i in \"$GUIX_EXTRA_PROFILES\"/*; do"
                            "  profile=$i/$(basename \"$i\")"
                            "  if [ -f \"$profile\"/etc/profile ]; then"
                            "    GUIX_PROFILE=\"$profile\""
                            "    . \"$GUIX_PROFILE\"/etc/profile"
                            "  fi"
                            "  unset profile"
                            "done"

                            ;; Load Nix environment
                            "if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then"
                            "    . /run/current-system/profile/etc/profile.d/nix.sh"
                            "fi")))

(define (augment-path-string . paths)
  ;; Generate a valid string to augment PATH with
  ;; all paths passed
  (apply string-append (append
                        (map
                         (lambda (path)
                           (string-append
                            path
                            ":"))
                         paths)
                        '("$PATH"))))

(define-syntax-rule (augment-path! path ...)
  (cons "PATH" (augment-path-string path ...)))


(define (home-desktop-environment-variables-service _)
  `(;; Many build scripts expect CC to contain the compiler command
    ("CC" . "gcc")

    ;; Make Flatpak apps visible to launcher
    ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")

    ;; Help Firefox with my timezone
    ("TZ" . "America/Sao_Paulo")
    
    ;; We're in Emacs, yo
    ("VISUAL" . "emacsclient")
    ("EDITOR" . "$VISUAL")

    ;; SSL certificates
    ("SSL_CERT_DIR" . "$HOME/.guix-home/profile/etc/ssl/certs")
    ("SSL_CERT_FILE" . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
    ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
    ("CURL_CA_BUNDLE" . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")

    ;; Augment PATH
    ,(augment-path!
      "$HOME/.local/bin"
      "$HOME/.yarn/bin"
      "$HOME/.cargo/bin")))


(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "Install and configure Desktop Environment")
                (extensions
                  (list (service-extension
                          home-profile-service-type
                          home-desktop-profile-service)
                        (service-extension
                          home-shell-profile-service-type
                          home-desktop-files-service)
                        (service-extension
                          home-environment-variables-service-type
                          home-desktop-environment-variables-service)
                        (service-extension
                          home-xdg-mime-applications-service-type
                          home-desktop-xdg-mime-applications-service)))
                (default-value #f)))
