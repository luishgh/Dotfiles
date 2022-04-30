(in-package #:nyxt-user)

;; Configure Webpage Colors
(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#282a36"
         :color "#f8f8f2")))))))

;; Configure Prompt Section
(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((body
               :background-color "#282a36"
               :color "#f8f8f2")
              ("#prompt-area"
               :background-color "#282a36")
              ("#input"
               :background-color "#6272a4"
               :color "#f8f8f2")
              (".source-name"
               :color "#f8f8f2"
               :background-color "#bd93f9")
              (".source-content"
               :background-color "#282a36")
              (".source-content th"
               :border "1px solid #bd93f9"
               :background-color "#282a36")
              ("#selection"
               :background-color "#44475a"
               :color "#f8f8f2")
              (.marked :background-color "#ff5555"
                       :font-weight "bold"
                       :color "#f8f8f2")
              (.selected :background-color "#282a36"
                         :color "#f8f8f2")))))))

(define-configuration internal-buffer
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((title
         :color "#ff79c6")
        (body
         :background-color "#21222C"
         :color "#f8f8f2")
        (hr
         :color "#44475a")
        (a
         :color "#6272a4")
        (.button
         :color "#f8f8f2"
         :background-color "#44475a")))))))

;; Configure History Tree Mode
(define-configuration nyxt/history-tree-mode:history-tree-mode
  ((nyxt/history-tree-mode::style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#21222C"
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "#50fa7b")
        ("ul li::before"
         :background-color "#f8f8f2")
        ("ul li::after"
         :background-color "#f8f8f2")
        ("ul li:only-child::before"
         :background-color "#f8f8f2")))))))

;; Highlight Hint Buttons
(define-configuration nyxt/web-mode:web-mode
  ((nyxt/web-mode:highlighted-box-style
    (cl-css:css
     '((".nyxt-hint.nyxt-highlight-hint"
        :background "#ff5555")))
    :documentation "The style of highlighted boxes, e.g. link hints.")))

;; Configure StatusLines Styles
(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '(("#controls"
               :border-top "1px solid #6272a4"
               :background-color "#21222C")
              ("#url"
               :background-color "#21222C"
               :color "#f8f8f2"
               :border-top "1px solid #6272a4")
              ("#modes"
               :background-color "#21222C"
               :border-top "1px solid #6272a4")
              ("#tabs"
               :background-color "#6272a4"
               :color "#f8f8f2"
               :border-top "1px solid #6272a4")))))))

(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "#21222C !important"
                :background-image "none !important"
                :color "#f8f8f2")
               (a
                :background-color "#21222C !important"
                :background-image "none !important"
                :color "#556B2F !important"))))))

;; Set StatusLines Mode Icons
(define-configuration status-buffer ((glyph-mode-presentation-p t)))
(define-configuration nyxt/force-https-mode:force-https-mode ((glyph "")))
(define-configuration nyxt/blocker-mode:blocker-mode ((glyph "")))
(define-configuration nyxt/proxy-mode:proxy-mode ((glyph "")))
(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode  ((glyph "")))
(define-configuration nyxt/certificate-exception-mode:certificate-exception-mode ((glyph "")))
(define-configuration nyxt/style-mode:style-mode ((glyph "")))
(define-configuration nyxt/help-mode:help-mode ((glyph "")))
(define-configuration nyxt/web-mode:web-mode ((glyph "ω")))
(define-configuration nyxt/auto-mode:auto-mode ((glyph "α")))

;;;; Set StatusLines URL Icons
;;(defun laconic-format-status-load-status (buffer)
;;  (if (web-buffer-p buffer)
;;      (case (slot-value buffer 'nyxt::load-status)
;;        (:unloaded "∅")
;;        (:loading "∞")
;;        (:finished ""))
;;      ""))
;;
;;;; Remove https/www from URL
;;(defun laconic-format-status-url (buffer)
;;  (markup:markup
;;   (:span
;;       (format nil "~a ~a"
;;               (laconic-format-status-load-status buffer)
;;               (ppcre:regex-replace-all
;;                "(https://|www\\.|/$)"
;;                (render-url (url buffer))
;;                "")))))
;;
;;;; Set StatusLines Webpage Loadtime
;;(defun laconic-format-status-modes (buffer window)
;;  (str:concat
;;   (format-status-modes buffer window)
;;   " | "
;;   (format nil "~d:~d"
;;           (mod (+ 5 (local-time:timestamp-hour (local-time:now))) 24)
;;           (local-time:timestamp-minute (local-time:now)))))
;;
;; Configure StatusLines Design
;; (defun laconic-format-status (window)
;;  (flet ((input-indicating-background ()
;;           (format nil "background-color: ~:[#50fa7b~;#ff5555~]"
;;                   (or (current-mode 'vi-insert)
;;                       (current-mode 'input-edit)))))
;;    (let ((buffer (current-buffer window)))
;;      (markup:markup
;;       (:div :id "container"
;;             (:div :id "controls"
;;                   :style (input-indicating-background)
;;                   (markup:raw ""))
;;             (:div :class "arrow arrow-right"
;;                   :style (input-indicating-background) "")
;;             (:div :id "url"
;;                   (markup:raw
;;                    (laconic-format-status-url buffer)))
;;             (:div :class "arrow arrow-right"
;;                   :style "background-color:#21222C" "")
;;             (:div :id "tabs"
;;                   (title buffer))
;;             (:div :class "arrow arrow-left"
;;                   :style "background-color:#21222C" "")
;;             (:div :id "modes"
;;                   :title (nyxt::list-modes buffer)
;;                   (laconic-format-status-modes buffer window)))))))

;; Define Current Configuration
;; (define-configuration window
;;  ((status-formatter #'laconic-format-status)))

