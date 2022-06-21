(define-module (guix-home services xdg)
  #:use-module (guix gexp)

  #:use-module (gnu home services)
  #:use-module (gnu home services xdg))

(define xdg-configuration
  (home-xdg-mime-applications-configuration
          (default '((inode/directory . emacs-dired)
                     (application/x-bittorrent . transmission)
                     (text/plain . emacs)
                     (x-scheme-handler/magnet torrent)))
          (desktop-entries
           (list (xdg-desktop-entry
                  (file "emacs-mailto")
                  (name "Handler for mailto:")
                  (type 'application)
                  (config
                   `((exec
                      . ,(program-file
                          "emacs-mailto"
                          #~(system
                             (string-append
                              "emacs --eval '(browse-url-mail \""
                              (car (cdr (command-line))) "\")'")))))))
                 (xdg-desktop-entry
                  (file "transmission")
                  (name "Bittorent client")
                  (type 'application)
                  (config
                   '((exec . "transmission-remote -a %U"))))
                 (xdg-desktop-entry
                  (file "emacs-dired")
                  (name "Emacs Dired file manager")
                  (type 'application)
                  (config
                   '((exec . "emacsclient -c -a emacs %u"))))
                 (xdg-desktop-entry
                  (file "emacs")
                  (name "Self-documenting and extensible text editor")
                  (type 'application)
                  (config
                   '((exec . "emacsclient -c -a emacs %u"))))))))

(define home-xdg-service
  (service home-xdg-mime-applications-service-type xdg-configuration))
