#lang racket

(define (empty-env) '())

(define (extend-env var val env)
  (cons (cons var val) env))



(define (apply-env env search-var)
  (cond
    [(null? env) (error 'apply-env "No binding")]
    [(eqv? (caar env) search-var) (cdar env)]    
    [else (apply-env (cdr env) search-var)]))




;; En la gramatica es el no-terminal Program
(struct let-program ()
  #:transparent)

;; En la gramatica es el no-terminal Expression
(struct let-expression ()#:transparent)

;; En la gramtica los siguientes arboles corresponden a producciones de Program
(struct a-program let-program (exp1)
  #:transparent)

;; En la gramatica los siguientes tipos de arboles corresponden a producciones de Expression

(struct const-exp let-expression (num) #:transparent)

(struct diff-exp let-expression (exp1 exp2) #:transparent)

(struct zero-exp let-expression (exp1) #:transparent)

(struct if-exp let-expression (exp1 exp2 exp3) #:transparent)

(struct var-exp let-expression (var) #:transparent)

(struct let-exp let-expression (var exp1 body) #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; --- Extension del lenguaje LET--
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
(struct minus-exp let-expression (exp1) #:transparent)

(provide
 (contract-out
  (let-program? (-> any/c boolean?))
  (let-expression? (-> any/c boolean?))
  (struct const-exp ((num integer?)))
  (struct a-program ((exp1 let-expression?)))
  (struct diff-exp ((exp1 let-expression?) (exp2 let-expression?)))
  (struct zero-exp ((exp1 let-expression?)))
  (struct if-exp ((exp1 let-expression?) (exp2 let-expression?) (exp3 let-expression?)))
  (struct var-exp ((var let-expression?)))
  (struct let-exp ((var let-expression?) (exp1 let-expression?) (body let-expression?)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; --- Extension del lenguaje LET--
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
 (struct minus-exp ((exp1 let-expression?)))
 ))





