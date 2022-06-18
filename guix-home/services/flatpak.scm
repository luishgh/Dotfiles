(define-module (guix-home services flatpak)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages package-management)
  #:export (home-flatpak-service-type))

(define (home-flatpak-profile-service config)
  (list flatpak))

(define (home-flatpak-files-service config)
  (list (plain-file "flatpak-bindir.sh" "
if [ -n \"$XDG_DATA_HOME\" ] && [ -d \"$XDG_DATA_HOME/flatpak/exports/bin\" ]; then
  export PATH=\"$XDG_DATA_HOME/flatpak/exports/bin\":\"$PATH\"
elif [ -n \"$HOME\" ] && [ -d \"$HOME/.local/share/flatpak/exports/bin\" ]; then
  export PATH=\"$HOME/.local/share/flatpak/exports/bin\":\"$PATH\"
fi

if [ -d /var/lib/flatpak/exports/bin ]; then
  export PATH=/var/lib/flatpak/exports/bin:\"$PATH\"
fi
")))

(define home-flatpak-service-type
  (service-type (name 'home-flatpak)
                (description "Install and configure Flatpak.")
                (extensions
                 (list (service-extension
                        home-shell-profile-service-type
                        home-flatpak-files-service)
                       (service-extension
                        home-profile-service-type
                        home-flatpak-profile-service)))
                (default-value #f)))
