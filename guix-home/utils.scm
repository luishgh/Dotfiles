(define-module (guix-home utils)
  #:use-module (guix gexp)
  #:export (home-config-file))

(define-syntax home-config-file
  (syntax-rules ()
    ((_ file-name exps exps* ...)
     (apply mixed-text-file file-name
                      (map (lambda (item)
			     (if (string? item)
				 (string-append item "\n")
				 item))
			   (list exps exps* ...))))))
