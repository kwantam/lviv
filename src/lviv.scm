
;#lang r5rs
; Except that somehow DrRacket's implementation is case sensitive and stuff

; **************
; **** MISC ****
; **************

; splitAt, like the haskell function
; splitAt :: Int -> [a] -> ([a],[a])
(define (splitAt n lst)
  (letrec 
	((splitAtHlp (lambda (lst n hd)
				   (cond ((null? lst) (cons (reverse hd) '())) ; no more list to split
						 ((= n 0) (cons (reverse hd) lst))
						 (else 
						   (splitAtHlp (cdr lst) (- n 1) (cons (car lst) hd)))))))
	(splitAtHlp lst n '())))

; like iterate f lst !! x
(define (iterateN f n lst)
  (if (= n 0) lst (iterateN f (- n 1) (f lst))))

; iterate, but quit on empty list
(define (iterateNOrNull f n lst)
  (if (or (= n 0) (null? lst)) lst (iterateNOrNull f (- n 1) (f lst))))

; ***************
; **** STATE ****
; ***************

; an empty stack
(define emptyStack '())

; an empty environment
(define emptyEnv (cons '() '()))

; an interpreter state is just (stack . env)
(define emptyState (cons emptyStack emptyEnv))

; retrieve stack from the state
(define stGetStack car)

; retrieve env from the state
(define stGetEnv cdr)

; retrieve bindings from the env
(define stGetBindings cadr)
(define envGetBindings car)

; retrieve enclsure from the env
(define stGetEnclosure cddr)
(define envGetEnclosure cdr)

; is this the global environment?
(define (global? env) (eq? (envGetEnclosure env) '()))

; update environment
(define (stUpdateEnv oldState newEnv) (set-cdr! oldState newEnv))
; update stack
(define (stUpdateStack oldState newStack) (set-car! oldState newStack))

; ***************
; **** STACK ****
; ***************

; update stack, returning a value
; supplied function should return a value such that
; result is (car (f stack))
; newStk is (cdr (f stack))
(define (stStackUpd2 f state)
  (let* ((fValue (f (stGetStack state)))
		 (fResult (car fValue))
		 (fRemain (cdr fValue)))
	(stUpdateStack state fRemain)
	fResult))

; update stack, returning new stack
(define (stStackUpd f state)
  (let ((fValue (f (stGetStack state))))
	(stUpdateStack state fValue)
	fValue))

; push onto stack
(define (stackPush stack var) (cons var stack))
(define (stStackPush state var) 
  (stStackUpd (lambda (stack) (stackPush stack var)) state))

; stack depth
(define depth length)
(define (stStackDepth state) (depth (stGetStack state)))

; pop off stack
; when popping, return (cons <popped value> <new stack>)
; since we represent the stack as a list, pop is just
; the identity function
(define (stackPop stack)
  (if (> 0 (depth stack))
	  stack
	  (cons #f stack)))
(define (stStackPop state) (stStackUpd2 stackPop state))

; swap the 0th and 1st elements of the stack
(define (stackSwap stack)
  (if (> 1 (depth stack))
	  (cons #t (cons (cadr stack) (cons (car stack) (cddr stack))))
	  (cons #f stack)))
(define (stStackSwap state) (stStackUpd2 stackSwap state))

; drop the 0th element
(define (stackDrop stack)
  (if (> 0 (depth stack))
	  (cons #t (cdr stack))
	  (cons #f stack)))
(define (stStackDrop state) (stStackUpd2 stackDrop state))

; clear the stack, i.e., replace it with emptyState
(define stackClear (lambda (x) emptyState))
(define (stStackClear state) (stStackUpd stackClear state))

; drop N elements after the 0th (which tells us N)
; return (#f,stack) if 0th element is not a number
; return (#t,stack) if 0th element is a number
; if length of stack is shorter than n, drop the whole thing
(define (stackDropN stack)
  (let* ((popVal (stackPop stack))
		 (popRes (car popVal))
		 (popRem (cdr popVal)))
	(if (number? popRes)
		(cons #t (iterateNOrNull stackDrop popRes popRem))
		(cons #f popRem))))
(define (stStackDropN state) (stStackUpd2 stackDropN state))
