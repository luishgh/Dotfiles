(define-module (guix-home services emacs)
  #:use-module (guix gexp)

  #:use-module (guix-home utils)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:export (home-emacs-service-type))

(define emacs-mailto-file
  (program-file
   "emacs-mailto"
   #~(system
      (string-append
       #$(home-extra-profile-executable-path emacs emacsclient)
       " -c -a emacs --eval '(message-mailto \""
       (cadr (command-line)) "\")'"))))

(define (home-emacs-files-service config)
  (list (list ".local/bin/emacs-mailto"
              emacs-mailto-file)))

(define (home-emacs-xdg-mime-applications-configuration config)
  (home-xdg-mime-applications-configuration
          (default '((inode/directory . emacs-dired.desktop)
                     (text/plain . emacs.desktop)))
          (desktop-entries
           (list (xdg-desktop-entry
                  (file "emacs-mailto")
                  (name "Handler for mailto:")
                  (type 'application)
                  (config
                   `((exec
                      . ,emacs-mailto-file))))
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

;; TODO: decide better name as this service doesn't install Emacs (I prefer to maintain it in a separate profile)
(define home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Configure emacs.")
                (extensions
                 (list (service-extension
                        home-xdg-mime-applications-service-type
                        home-emacs-xdg-mime-applications-configuration)
                       (service-extension
                        home-files-service-type
                        home-emacs-files-service)))
                (default-value #f)))
