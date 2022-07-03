(define-module (guix-home services xdg)
  #:use-module (guix gexp)

  #:use-module (gnu home services)
  #:use-module (gnu home services xdg))

(define xdg-configuration
  (home-xdg-mime-applications-configuration
   (default '((application/x-bittorrent . transmission)
              (x-scheme-handler/magnet torrent)))
   (desktop-entries
    (list (xdg-desktop-entry
           (file "transmission")
           (name "Bittorent client")
           (type 'application)
           (config
            '((exec . "transmission-remote -a %U"))))))))

(define home-xdg-service
  (service home-xdg-mime-applications-service-type xdg-configuration))
