#lang racket
; Tree Datatypes and parser definitions

(define parse
  (lambda (input)
    (cond
      ((number? input) (new-lit-exp input)); A
      ((symbol? input) (new-var-ref input)); B
      ((not (pair? input)) (error 'parse "Invalid syntax ~s" input))
      ((eq? 'if (car input)) (new-if-exp (map parse (cdr input)))); D
      ((eq? 'let (car input)) (new-let-exp (list
                                            (map car (cadr input))
                                            (map parse (map cadr (cadr input)))
                                            (parse (caddr input))))); E
      ((eq? 'lambda (car input)) (new-lambda-exp (list
                                                  (cadr input)
                                                  (parse (caddr input))))); F
      ((eq? 'set! (car input)) (new-assign-exp (list
                                                (cadr input)
                                                (parse (caddr input)))))
      ((eq? 'begin (car input)) (new-sequence-exp (map parse (cdr input)))); G
      ((eq? 'letrec (car input))
       (let ((ids (map car (cadr input)))
             (vals (map parse (map cadr (cadr input))))
             (body (parse (caddr input))))
         (make-letrec ids vals body))); H
      (else (new-app-exp (parse (car input)) (map parse (cdr input))))))); C

(provide parse)

; MiniSchemeA: lit-exp datatype
(define new-lit-exp (lambda (num) (list 'lit-exp num))); constructor
(define lit-exp? (lambda (x) (eq? 'lit-exp (car x)))); recognizer
(define lit-exp-num (lambda (x) (cadr x))); getter
(provide new-lit-exp lit-exp? lit-exp-num)

; MiniSchemeB: var-ref datatype
(define new-var-ref (lambda (sym) (list 'var-ref sym))); constructor
(define var-ref? (lambda (x) (eq? 'var-ref (car x)))); recognizer
(define var-ref-sym (lambda (x) (cadr x))); getter
(provide new-var-ref var-ref? var-ref-sym)

; MiniSchemeC: prim-proc, app-exp datatype
(define new-app-exp (lambda (proc args) (append (list 'app-exp proc) args))); constructor
(define app-exp? (lambda (x) (eq? 'app-exp (car x)))); recognizer
(define app-exp-proc (lambda (x) (cadr x))); procedure getter
(define app-exp-args (lambda (x) (cddr x))); args getter
(provide new-app-exp app-exp? app-exp-proc app-exp-args)

; MiniSchemeD: if-exp datatype
(define new-if-exp (lambda (exp) (cons 'if-exp exp))); constructor
(define if-exp? (lambda (x) (eq? 'if-exp (car x)))); recognizer
(define if-cond (lambda (x) (cadr x))); condition getter
(define if-true-arg (lambda (x) (caddr x))); true-arguments getter
(define if-false-arg (lambda (x) (cadddr x))); false-arguments getter
(provide new-if-exp if-exp? if-cond if-true-arg if-false-arg)

; MiniSchemeE: let-exp datatype
(define new-let-exp (lambda (exp) (cons 'let-exp exp))); constructor
(define let-exp? (lambda (x) (eq? 'let-exp (car x)))); recognizer
(define let-syms (lambda (x) (cadr x))); list of syms
(define let-exps (lambda (x) (caddr x))); list of parsed exps
(define let-body (lambda (x) (cadddr x))); let body getter
(provide new-let-exp let-exp? let-syms let-exps let-body)

; MiniSchemeF: lambda-exp, closure datatype
(define new-lambda-exp (lambda (exp) (cons 'lambda exp))); constructor
(define lambda-exp? (lambda (x) (eq? 'lambda (car x)))); recognizer
(define lambda-syms (lambda (x) (cadr x))); syms getter
(define lambda-body (lambda (x) (caddr x))); parsed body getter
(provide new-lambda-exp lambda-exp? lambda-syms lambda-body)

(define new-closure (lambda (syms body env) (list 'closure syms body env))); constructor
(define closure? (lambda (x) (eq? 'closure (car x)))); recognizer
(define closure-parms (lambda (x) (cadr x))); syms getter
(define closure-body (lambda (x) (caddr x))); body getter
(define closure-env (lambda (x) (cadddr x))); env getter
(provide new-closure closure? closure-parms closure-body closure-env)

; MiniSchemeG: assign-exp, sequence-exp datatype
(define new-assign-exp (lambda (exp) (cons 'assign-exp exp))); constructor
(define assign-exp? (lambda (x) (eq? 'assign-exp (car x)))); recognizer
(define assign-sym (lambda (x) (cadr x))); sym getter
(define assign-val (lambda (x) (caddr x))); val getter
(provide new-assign-exp assign-exp? assign-sym assign-val)

(define new-sequence-exp (lambda (exp) (cons 'sequence-exp exp))); constructor
(define sequence-exp? (lambda (x) (eq? 'sequence-exp (car x)))); recognizer
(define sequence-exp (lambda (x) (cdr x))); exp getter
(provide new-sequence-exp sequence-exp? sequence-exp)

; MiniSchemeH: letrec recursion helper
(define letrec-helper
  (lambda (ids new-ids)
    (cond
      ((null? ids) null)
      (else
       (cons (new-assign-exp (list (car ids) (parse (car new-ids))))
             (letrec-helper (cdr ids) (cdr new-ids))))))); parse? 
    
(define make-letrec
  (lambda (ids vals body)
    (let ((zeros (map (lambda (x) (new-lit-exp x)) ids))
          (new-ids (map (lambda (x) (gensym)) ids)))
      (new-let-exp (list ids zeros
                   (new-let-exp (list new-ids vals
                                (new-sequence-exp (append (letrec-helper ids new-ids) (list body))))))))))
