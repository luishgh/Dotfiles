(define-module (guix-home services mcron)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services mcron)

  #:use-module (guix-home utils))

  ;; #:export ())

(define home-mcron-jobs
  (list
   #~(job
      next-day
      "pass git pull && pass git push -u --all"
      "Password sync cron (daily)")))

(define home-mcron-service
  (simple-service
   'lhgh/home-mcron
   home-mcron-service-type
   home-mcron-jobs))
