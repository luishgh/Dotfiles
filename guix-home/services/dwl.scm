(define-module (guix-home services dwl)
  #:use-module (guix gexp)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:export (home-dwl-services))

;; Import the service
(use-modules (dwl-guile home-service)
             (dwl-guile patches)) ; import if you want to apply patches dynamically

;; Import the service
(use-modules (dtao-guile home-service))

(define %tags-and-layout
  (append
   (map
    (lambda (tag)
      (let ((str (string-append "^p(8)" 
                                (number->string tag) 
                                ;tag
                                "^p(8)"))
            (index (- tag 1)))
      (dtao-block
       (interval 0)
       (events? #t)
       (click `(match button
                 (0 (dtao:view ,index))))
       (render `(cond
                 ((dtao:selected-tag? ,index)
                  ,(string-append "^bg(#ffcc00)^fg(#191919)" str "^fg()^bg()"))
                 ((dtao:urgent-tag? ,index)
                  ,(string-append "^bg(#ff0000)^fg(#ffffff)" str "^fg()^bg()"))
                 ((dtao:active-tag? ,index)
                  ,(string-append "^bg(#323232)^fg(#ffffff)" str "^fg()^bg()"))
                 (else ,str))))))
    (iota 9 1))
   (list
    (dtao-block
     (events? #t)
     (click `(dtao:next-layout))
     (render `(string-append "^p(4)" (dtao:get-layout)))))))

(define dtao-configuration
  (home-dtao-guile-configuration
    (auto-start? #f)
    (config
      (dtao-config
        ;; A font string in fcft format.
        (font "monospace:style=bold:size=12")
        ;; Read `root', `border' and `text' colors from dwl-guile.
        (background-color "111111AA")
        (border-color "333333FF")
        (foreground-color "FFFFFFFF")
        (padding-left 8)
        (padding-right 8)
        (padding-top 2)
        (padding-bottom 2)
        ;; Request an exclusive zone for the bar to prevent overlapping.
        (exclusive? #t)
        ;; Layer to render the bar in (LAYER-TOP, LAYER-BOTTOM, LAYER-OVERLAY, LAYER-BACKGROUND).
        (layer 'LAYER-BOTTOM)
        ;; Render the bar at the bottom of the screen.
        (bottom? #f)
        ;; Height of the bar in pixels. Set to #f for automatic height based on font size.
        (height #f)
        ;; Additional spacing on each side of the delimiter string.
        (block-spacing 0)
        (left-blocks %tags-and-layout)
        (center-blocks (list
                         (dtao-block
                           (events? #t)
                           (render `(dtao:title)))))
        (right-blocks (list
                       (dtao-block
                        (interval 1)
                        (render `(strftime "%A, %d %b (w.%V) %T" (localtime (current-time)))))))
        (modules '((ice-9 match)
                   (ice-9 popen)
                   (ice-9 rdelim)
                   (srfi srfi-1)))))))

(define dwl-configuration
  (home-dwl-guile-configuration
    (package
      (patch-dwl-guile-package dwl-guile
                               #:patches (list %patch-xwayland)))
    ;; The default sets GDK_BACKEND, which breaks emacs ¯\_(ツ)_/¯
    (environment-variables 
      `(("XDG_CURRENT_DESKTOP" . "dwl")
        ("XDG_SESSION_TYPE" . "wayland")
        ("MOZ_ENABLE_WAYLAND" . "1")
        ("ELM_ENGINE" . "wayland_egl")
        ("ECORE_EVAS_ENGINE" . "wayland-egl")
        ("_JAVA_AWT_WM_NONREPARENTING" . "1")))
    (auto-start? #f)
    (native-qt? #f)
    (config
      `(((setq inhibit-defaults? #t)
         (dwl:start-repl-server)
         (setq tags (map number->string (iota 9 1)))
         (dwl:set-tty-keys "C-M")
         (dwl:set-tag-keys "s" "s-S")
         (set-keys "s-<space>" 'dwl:toggle-floating
                   "s-<return>" '(dwl:spawn "foot")
                   "s-p" '(dwl:spawn "bemenu-run")
                   "s-e" '(dwl:spawn "emacsclient" "-c")
                   "s-j" '(dwl:focus-stack 1)
                   "s-k" '(dwl:focus-stack -1)
                   "s-l" '(dwl:change-master-factor 0.05)
                   "s-h" '(dwl:change-master-factor -0.05)
                   "s-<page-up>" '(dwl:change-masters 1)
                   "s-<page-down>" '(dwl:change-masters -1)
                   "s-t" '(dwl:cycle-layout 1)
                   "s-<left>" '(dwl:focus-monitor 'DIRECTION-LEFT)
                   "s-<right>" '(dwl:focus-monitor 'DIRECTION-RIGHT)
                   "s-<up>" '(dwl:focus-monitor 'DIRECTION-UP)
                   "s-<down>" '(dwl:focus-monitor 'DIRECTION-DOWN)
                   "s-S-<left>" '(dwl:tag-monitor 'DIRECTION-LEFT)
                   "s-S-<right>" '(dwl:tag-monitor 'DIRECTION-RIGHT)
                   "s-S-<up>" '(dwl:tag-monitor 'DIRECTION-UP)
                   "s-S-<down>" '(dwl:tag-monitor 'DIRECTION-DOWN)
                   "s-S-c" 'dwl:kill-client
                   "s-<tab>" 'dwl:view
                   "s-S-0" '(dwl:view 0) ;; 0 will show all tags
                   "s-f" 'dwl:toggle-fullscreen
                   "S-s-q" 'dwl:quit
                   "s-<mouse-left>" 'dwl:move
                   "s-<mouse-middle>" 'dwl:toggle-floating
                   "s-<mouse-right>" 'dwl:resize)
         (set-layouts 'default "[]=" 'dwl:tile
                      'monocle "|M|" 'dwl:monocle)
         ;; Launch helpful programs after init
         (add-hook! dwl:hook-startup
                    (lambda ()
                      ;(dwl:spawn "setxkbmap" "-option" "ctrl:nocaps")
                      (dwl:spawn "dunst")
                      ;(dwl:spawn "nm-applet" "--indicator")
                      (dwl:spawn "dtao-guile" "-c" ".config/dtao-guile/config.scm")
                      (dwl:spawn "emacs" "--daemon"))))))))

(define (home-dwl-environment-variables-service _)
  '(("xdg_current_desktop" . "dwl")))

(define home-dwl-services
  (list (service home-dwl-guile-service-type
                 dwl-configuration)
        ;; (simple-service
        ;;   'home-dwl-environment-variables
        ;;   home-environment-variables-service-type
        ;;   home-dwl-environment-variables-service)
        (service home-dtao-guile-service-type
                 dtao-configuration)))
