#lang racket
; eval-exp definition

(require "env.rkt")
(require "parse.rkt")

(define eval-exp
  (lambda (tree env)
    (cond
      ((lit-exp? tree) (lit-exp-num tree)); A
      ((var-ref? tree) (unbox (lookup env (var-ref-sym tree)))); B
      ((app-exp? tree) (apply-proc
                        (eval-exp (app-exp-proc tree) env)
                        (map (lambda (t) (eval-exp t env)) (app-exp-args tree)))); C
      ((if-exp? tree) (cond
                        ((eq? 0 (eval-exp (if-cond tree) env))
                         (eval-exp (if-false-arg tree) env))
                        ((eq? 'False (eval-exp (if-cond tree) env))
                         (eval-exp (if-false-arg tree) env))
                        ((eq? 'True (eval-exp (if-cond tree) env))
                         (eval-exp (if-true-arg tree) env))
                        (else (eval-exp (if-true-arg tree) env)))); D
      ((let-exp? tree) (eval-exp (let-body tree)
                                 (extended-env (let-syms tree)
                                               (map (lambda (t) (eval-exp t env))
                                               (let-exps tree))
                                               env))); E
      ((lambda-exp? tree) (new-closure
                           (lambda-syms tree)
                           (lambda-body tree)
                           env)); F
      ((assign-exp? tree) (let
                           ((b (lookup env (assign-sym tree)))
                           (value (eval-exp (assign-val tree) env)))
                            (set-box! b value)))
      ((sequence-exp? tree) (let ((exps (sequence-exp tree)))
                              (cond
                                ((null? exps) null)
                                ((null? (cdr exps)) (eval-exp (car exps) env))
                                (else (begin
                                        (eval-exp (car exps) env)
                                        (eval-exp (new-sequence-exp (cdr exps)) env)))))); G
      (else (error 'eval-exp "Invalid tree: ~s" tree)))))

; C
(define apply-proc
  (lambda (p arg-values)
    (cond
      ((prim-proc? p) (apply-primitive-op (prim-proc-symbol p) arg-values))
      ((closure? p) (eval-exp (closure-body p)
                                  (extended-env
                                   (closure-parms p)
                                   arg-values
                                   (closure-env p)))); F
      (else (error 'apply-proc "Bad procedure: ~s" p)))))

(define apply-primitive-op
  (lambda (op arg-values)
    (cond
      ((eq? op '+) (+ (car arg-values) (cadr arg-values)))
      ((eq? op '-) (- (car arg-values) (cadr arg-values)))
      ((eq? op '*) (* (car arg-values) (cadr arg-values)))
      ((eq? op '/) (/ (car arg-values) (cadr arg-values)))
      
      ((eq? op 'add1) (+ (car arg-values) 1))
      ((eq? op 'sub1) (- (car arg-values) 1))
      ((eq? op 'minus) (- 0 (car arg-values)))
      
      ((eq? op 'list) (apply list arg-values))
      ((eq? op 'build) (cons (car arg-values) (cadr arg-values)))
      ((eq? op 'first) (car (car arg-values)))
      ((eq? op 'rest) (cdr (car arg-values)))
      ((eq? op 'empty?) (null? (car arg-values)))
      
      ((eq? op 'equals?) (if (eqv? (car arg-values) (cadr arg-values)) True False))
      ((eq? op 'gt?) (if (> (car arg-values) (cadr arg-values)) True False))
      ((eq? op 'geq?) (if (>= (car arg-values) (cadr arg-values)) True False))
      ((eq? op 'lt?) (if (< (car arg-values) (cadr arg-values)) True False))
      ((eq? op 'leq?) (if (<= (car arg-values) (cadr arg-values)) True False))
      (else (error 'apply-primitive-op "Bad procedure: ~s" op)))))

(provide eval-exp)