#lang racket
(require "ast.rkt")
(require "env.rkt")
(require "vals.rkt")


(define (value-of expr env)
  (cond ((const-exp? expr)
         (num-val(const-exp-num expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;   
        ((zero-exp?  expr)
         (bool-val (zero? (expval->num (value-of (zero-exp-exp1 expr)env)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        
        ((if-exp? expr)
         (let((val1 (value-of (if-exp-exp1 expr) env )))
           (if (eqv?(expval->bool val1)#t)
               (value-of (if-exp-exp2 expr) env)
               (value-of (if-exp-exp3 expr) env ))))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        
        ((diff-exp? expr)
         (let ((val1 (value-of (diff-exp-exp1 expr)env))
               (val2 (value-of (diff-exp-exp2 expr)env)))
           (num-val(-(expval->num val1)
                     (expval->num val2 )))))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ((var-exp? expr)
         (apply-env env ( var-exp-var expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ((let-exp? expr)
         (let ((val1 (value-of (let-exp-exp1 expr) env)))
           (value-of (let-exp-body expr) (extend-env (let-exp-var expr) val1 env))))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; --- Extension del lenguaje LET--
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
        ((minus-exp expr
                    (num-val(-(expval->num (value-of expr env))))))))



(define (init-env)
  (extend-env 'imperio-romano (bool-val #f)(extend-env'satan (num-val 666)
                                                      (empty-env))))


(define (value-of-program prgm)
  (match prgm
    ((a-program expr)
     (value-of expr (init-env)))
    (else
     (error "no programa "))))


(provide
 (contract-out
  (value-of-program (-> let-program? expval?))
  (value-of (-> let-expression?  environment? expval?))))


 
