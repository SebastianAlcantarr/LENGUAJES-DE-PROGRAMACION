#lang racket


(struct expval () #:transparent)

(struct num-val expval (num) #:transparent)

(struct bool-val expval (bool) #:transparent)


(define (expval->num val)
  (if (num-val? val)
      (num-val-num val)
      (error "valor no esperado")))

(define (expval->bool val)
  
  (if (bool-val? val)

      (bool-val-bool val)
      
      (error "valor no esperado")))


(provide
 (contract-out
  [expval? (-> any/c boolean?)]
  [expval (-> any)]
  (num-val (-> integer? expval?))
  (bool-val (-> boolean? expval?))
  (expval->num (-> expval? integer?))
  (expval->bool (-> expval? boolean?))
  ))







