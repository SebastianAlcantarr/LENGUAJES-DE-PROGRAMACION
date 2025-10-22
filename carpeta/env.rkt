#lang racket

(require "vals.rkt")

(struct environment () #:transparent)
(struct empty-env environment () #:transparent)

(struct extend-env environment (var val parent) #:transparent)



(define (apply-env env search-var)
  (if (empty-env? env)
      (error "No binding")
      (let ((var (extend-env-var env))
            (val (extend-env-val env))
            (parent (extend-env-parent env)))
        (if (eq? var search-var)
            val
            (apply-env parent search-var)))))


(provide
 (contract-out
  (environment? (-> any/c boolean? ))
  (empty-env (-> environment?))
  (apply-env (-> environment? symbol? expval?))
  (extend-env (-> symbol? expval? environment? environment?))
  ))



