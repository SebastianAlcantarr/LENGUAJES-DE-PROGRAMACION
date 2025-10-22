#lang racket
(require "ast.rkt")
(require "env.rkt")
(require "vals.rkt")


(define (value-of expr env)
  (cond ((const-exp? expr)
         (num-val(const-exp-num expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ((zero-exp?  expr)
         (bool-val (zero? (expval->num (value-of (zero-exp-exp1 expr)env)))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        
        ((if-exp? expr)
         (let((val1 (value-of (if-exp-exp1 expr) env )))
           (if (eqv?(expval->bool val1)#t)
               (value-of (if-exp-exp2 expr) env)
               (value-of (if-exp-exp3 expr) env ))))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        
        ((diff-exp? expr)
         (let ((val1 (value-of (diff-exp-exp1 expr)env))
               (val2 (value-of (diff-exp-exp2 expr)env)))
           (num-val(-(expval->num val1)
                     (expval->num val2 )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ((var-exp? expr)
         (apply-env env ( var-exp-var expr)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ((let-exp? expr)
         (let ((val1 (value-of (let-exp-exp1 expr) env)))
           (value-of (let-exp-body expr) (extend-env (let-exp-var expr) val1 env))))

;;;;;;;;;;;;;;;;;; --------- LISTAS -------------------------------;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ((cons-exp? expr)
         (let ((val1 (value-of (cons-exp-car expr) env))
               (val2 (value-of (cons-exp-cdr expr) env)))
           (pair-val val1 val2)))

        ((empty-exp? expr)
         (empty-val))


        ((car-exp? expr)
         (let ((val (value-of (car-exp-exp1 expr) env)))
           (let ([pair (expval->pair val)])
             (pair-val-car pair))))


        ((cdr-exp? expr)
         (let ((val (value-of (cdr-exp-exp1 expr)env)))
           (let ((pair (expval->pair val)))
             (pair-val-cdr pair))))

        ((null-exp? expr )
         (let ((val (value-of (null-exp-exp1 expr )env)))
           (expval->empty val)
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



 
