#lang racket


(struct expval () #:transparent)

(struct num-val expval (num) #:transparent)

(struct bool-val expval (bool) #:transparent)
(struct pair-val expval (car cdr) #:transparent)
(struct empty-val expval () #:transparent)




(define (expval->num val)
  (if (num-val? val)
      (num-val-num val)
      (error "valor no esperado valor no numerico")))

(define (expval->bool val)
  (if (bool-val? val)
      (bool-val-bool val)
      (error "valor no esperado valor no booleano")))

(define (expval->pair val)
  (if (pair-val? val)
      val  
      (error "valor no esperado valor no es par ")))

(define (expval->empty val)
  (if (empty-val? val)
      (empty-val )
      (error "valor no esperado valor no vacio")))
  



(provide
 (contract-out
  [expval? (-> any/c boolean?)]
  [expval (-> any)]
  [num-val (-> integer? expval?)]
  [bool-val (-> boolean? expval?)]
  [pair-val (-> expval? expval? expval?)]
  [pair-val-car (-> pair-val? expval?)] 
  [pair-val-cdr (-> pair-val? expval?)]  
  [empty-val (-> expval?)]
  [expval->num (-> expval? integer?)]
  [expval->bool (-> expval? boolean?)]
  [expval->pair (-> expval? expval?)]
  (expval->empty (-> expval? expval?))
  ))




