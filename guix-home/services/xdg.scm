(define-module (guix-home services xdg)
  #:use-module (guix gexp)

  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)

  #:export (home-xdg-service))

(define xdg-configuration
  (home-xdg-mime-applications-configuration
   (default '((application/x-bittorrent . transmission.desktop)
              (application/x-pdf . zathura.desktop)
              (application/pdf . zathura.desktop)
              (x-scheme-handler/magnet torrent.desktop)))
   (desktop-entries
    (list (xdg-desktop-entry
           (file "transmission")
           (name "Bittorent client")
           (type 'application)
           (config
            '((exec . "transmission-remote -a %U"))))
          (xdg-desktop-entry
           (file "zathura")
           (name "PDF reader")
           (type 'application)
           (config
            '((exec . "zathura %U"))))))))

;; TODO: move XDG services to the places where applications are installed
(define home-xdg-service
  (service home-xdg-mime-applications-service-type xdg-configuration))
