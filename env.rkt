#lang racket
(provide environment? empty-env? extended-env? empty-env extended-env syms vals old-env
         the-empty-env lookup)


; datatype definition

(define environment? (lambda (e) (or (empty-env? e) (extended-env? e))))

(define empty-env? (lambda (e)
                     (cond
                       [(list? e) (eq? (car e) 'empty-env)]
                       [else #f])))

(define extended-env? (lambda (e)
                        (cond
                          [(list? e) (eq? (car e) 'extended-env)]
                          [else #f])))

(define empty-env (lambda ()
                    (list 'empty-env)))

(define extended-env (lambda (syms vals old-env)
                       (list 'extended-env syms (map box vals) old-env)))

(define syms (lambda (env)
               (cond
                 [(extended-env? env) (cadr env)]
                 [else (error 'syms "bad environment")])))

(define vals (lambda (env)
               (cond
                 [(extended-env? env) (caddr env)]
                 [else (error 'vals "bad environment")])))

(define old-env (lambda (env)
               (cond
                 [(extended-env? env) (cadddr env)]
                 [else (error 'old-env "bad environment")])))

(define the-empty-env (empty-env))

; prim-proc datatype
(define new-prim-proc (lambda (sym) (list 'prim-proc sym))); constructor
(define prim-proc? (lambda (x) (eq? 'prim-proc (car x)))); recognizer
(define prim-proc-symbol (lambda (p) (cadr p))); getter
(provide new-prim-proc prim-proc? prim-proc-symbol)

; define primitive operators
(define primitive-operators '(+ - * /
                                add1 sub1 minus
                                list build first rest empty?
                                equals? gt? lt? geq? leq? ))
(provide primitive-operators)

; add operators to env
(define init-env
  (extended-env primitive-operators
                (map new-prim-proc primitive-operators)
                (extended-env '(x y nil True False)
                              '(10 23 () True False)
                              the-empty-env)))
(define True 'True)
(define False 'False)
(provide init-env True False)

; lookup
(define lookup
  (lambda (environment symbol)
    (cond
      ((null? (syms environment))
       (if (empty-env? (old-env environment))
           (error 'apply-env "No binding for ~s" symbol)
           (lookup (old-env environment) symbol)))
      ((eq? symbol (car (syms environment)))(car (vals environment)))
      (else (lookup
             (list 'extended-env
                   (cdr (syms environment))
                   (cdr (vals environment))
                   (old-env environment))
                   symbol)))))
    