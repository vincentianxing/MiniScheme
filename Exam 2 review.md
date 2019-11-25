# Exam 2 review
#cs/275 11/23/19

## Programming language
### Environments


### State


### Static & Dynamic binding of free vars in functions


### Procedure calling mechanisms
* Call-by-value
* Call-by-reference
* Call-by-name

## Interpreter project
```scheme
(empty-env ‘()) 
	(list ‘empty-env))

(extended-env syms vals old-env)
	(list ‘extended-env syms (map box vals) old-env)))

(lookup env sym)
;recurse on (cdr syms) and (cdr vals) in current env
;look-up in old-env if sym no found in current env
(define lookup
  (lambda (environment symbol)
    (cond
      ((null? (syms environment))
       (if (empty-env? (old-env environment))
           (error 'apply-env "No binding for ~s" symbol)
           (lookup (old-env environment) symbol)))
      ((eq? symbol (car (syms environment)))
				(car (vals environment)))
      (else (lookup
             (list 'extended-env
                   (cdr (syms environment))
                   (cdr (vals environment))
                   (old-env environment))
                   symbol)))))
```

### Datatypes
* provide constructor, recognizer, accessor
```scheme
;lit-exp: parse number
(list ‘lit-exp num)
(cadr get-num)
;var-ref: parse symbol
(list ‘var-ref sym)
(cadr get-ref)
;app-exp: parse (exp exp)
(lambda (proc args) (append (list ‘app-exp proc) args))
(cadr get-proc)
(cddr get-args)
	;prim-proc: defined in env with primitive operator extended
	(list ‘prim-proc sym)
	(cadr get proc-symbol)
		;primitive operators
		+ - * / list bulid first rest empty equals? gt? lt?
;if-exp
(cons ‘if-exp exp)
(cadr get-condition)
(caddr get-true-arg)
(cadddr get-false-arg)
;let-exp
(cons ‘let-exp exp)
(cadr get-syms)
(caddr get-parsed-exps)
(cadddr get-body)
;lambda-exp
(cons ‘lambda exp)
(cadr get-syms)
(caddr get-parsed-body)
;closure
(list ‘closure syms body env)
(cadr get-parms)
(caddr get-body)
(cadddr get-env)
;assign-exp: parse set!
(cons ‘assign-exp exp)
(cadr get-sym)
(caddr get-val)
;sequence-exp: parse begin
(cons ‘sequence-exp exp)
(cdr get-exps)
```

### Parse
```scheme
(parse input)

;number
(new-lit-exp input)
;symbol
(new-var-ref input)
;not-pair
error, “invalid syntax"
;(if exp exp exp)
(new-if-exp (map parse (cdr input))) 
;(let (bindings) exp)
(new-let-exp (list
				(map car (cadr input))
				(map parse (map cadr (cadr input)))
				(parse (caddr input))))
;lambda
(new-lambda-exp (list
					(cadr input)
					(parse (caddr input)))
;set!
(new-assigin-exp (list
					(cadr input)
					(parse (caddr input)))
;begin
(new-sequence-exp (map parse (cdr input)))
;letrec
(make-letrec ids vals body)
	;make-letrec
	(new-let-exp (list ids zeros
		(new-let-exp (list new-ids vals
			(new-sequence-exp (append (ids newids) (list body))
;else procedure applications
(new-app-exp (parse (car input)) (map parse (cdr input)))
```

### Evaluate
```scheme
(eval-exp tree env)

;lit-exp
(get-num tree)
;var-ref
(unbox (lookup env (get-ref tree)))
;app-exp
(apply-proc
	(eval-exp (get-proc tree) env)
	(map (lambda (t) (eval-exp t env)) (get-args tree)))
	;apply-proc
	(apply-proc p arg-values)
	(prim-proc? p) (apply-primitive-op 
						(proc-symbol p) arg-values))
	(closure? p) (eval-exp (get-body p)
					(extended-env
						(get-parms p)
						arg-values
						(get-env p))))
	(else (error, “bad procedure”
;if-exp
0/‘False (eval-exp (get-false-arg tree) env)
else/‘True (eval-exp (get-true-arg tree) env)
;let-exp
(eval-exp (get-body tree)
	(extended-env 
				(get-syms tree)
				(map (lambda (t) (eval-exp t env))
					(get-exps tree))))
				env)))
;lambda-exp
(new-closure (get-syms tree) (get-body tree) env)
;assign-exp
(set-box!
	(lookup env (get-sym tree))
	(eval-exp (get-val tree) env)
;sequence-exp
(let exps (get-exps tree))
(null? exps) null
(null? (cdr exps)) (eval-exp (car exps) env)
(else (begin
	(eval-exp (car exps) env)
	(eval-exp (new-sequence-exp (cdr exps)) env)))
;else 
error, “invalid tree”
```

* Change environment datatype definition