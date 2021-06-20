(in-package #:nyxt-user)

(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#161821"
         :color "#C792EA")))))))

(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((body
               :background-color "#161821"
               :color "white")
              ("#prompt-area"
               :background-color "#161821")
              ("#input"
               :background-color "white")
              (".source-name"
               :color "#161821"
               :background-color "#c792ea")
              (".source-content"
               :background-color "#161821")
              (".source-content th"
               :border "1px solid #c792ea"
               :background-color "#161821")
              ("#selection"
               :background-color "#FF5370"
               :color "#161821")
              (.marked :background-color "#F78C6C"
                       :font-weight "bold"
                       :color "white")
              (.selected :background-color "#84a0c6"
                         :color "white")))))))

(define-configuration internal-buffer
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((title
         :color "#CD5C5C")
        (body
         :background-color "#161821"
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "#c792ea")
        (.button
         :color "lightgray"
         :background-color "#c792ea")))))))

(define-configuration nyxt/history-tree-mode:history-tree-mode
  ((nyxt/history-tree-mode::style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#161821"
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "#c792ea")
        ("ul li::before"
         :background-color "white")
        ("ul li::after"
         :background-color "white")
        ("ul li:only-child::before"
         :background-color "white")))))))

(define-configuration nyxt/web-mode:web-mode
  ((nyxt/web-mode:highlighted-box-style
    (cl-css:css
     '((".nyxt-hint.nyxt-highlight-hint"
        :background "#FF5370")))
    :documentation "The style of highlighted boxes, e.g. link hints.")))

(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '(("#controls"
               :border-top "1px solid white")
              ("#url"
               :background-color "#161821"
               :color "white"
               :border-top "1px solid white")
              ("#modes"
               :background-color "#161821"
               :border-top "1px solid white")
              ("#tabs"
               :background-color "#c792ea"
               :color "#161821"
               :border-top "1px solid white")))))))

(define-configuration nyxt/style-mode:dark-mode
  ((style #.(cl-css:css
             '((*
                :background-color "#161821 !important"
                :background-image "none !important"
                :color "white")
               (a
                :background-color "#161821 !important"
                :background-image "none !important"
                :color "#c792ea !important"))))))
