
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
                         ((<= n 0) (cons (reverse hd) lst))
                         (else 
                           (splitAtHlp (cdr lst) (- n 1) (cons (car lst) hd)))))))
    (splitAtHlp lst n '())))

; like iterate f lst !! x
(define (iterateN f n lst)
  (if (= n 0) lst (iterateN f (- n 1) (f lst))))

; iterate, but quit on empty list
(define (iterateNOrNull f n lst)
  (if (or (= n 0) (null? lst)) lst (iterateNOrNull f (- n 1) (f lst))))

; general number to integer
(define (number->int num) (inexact->exact (floor num)))

; rollN circular shifts the first few elements of a stack
; first element becomes second, et cetera
(define (rollN n lst)
  (let ((splitLst (splitAt (- (min n (length lst)) 1) lst)))
    (cons (cadr splitLst) (append (car splitLst) (cddr splitLst)))))

; unrollN circular shifts like rollN, but the other direction
(define (unrollN n lst)
  (let ((splitLst (splitAt (min n (length lst)) lst)))
    (append (cdar splitLst) (cons (caar splitLst) (cdr splitLst)))))

; dupN duplicates the first n elements of a list
(define (dupN n lst)
  (let ((splitLst (splitAt (min n (length lst)) lst)))
    (append (car splitLst) (append (car splitLst) (cdr splitLst)))))

; flip function of two variables
(define (flip f) (lambda (x y) (f y x)))

; pickN - like a consing list-ref but uses 1-based indexing
; attempts to never produce an error
(define (pickN n lst)
  (cond ((< n 1) lst)
        ((> n (length lst)) lst)
        (else (cons (list-ref lst (- n 1)) lst))))

; eLeft and eRight are like the Either monad
; eRight signals success, eLeft signals failure
(define (eLeft msg) (cons #f msg))
(define (eRight msg) (cons #t msg))
(define (eRight? either) (and (pair? either) (car either)))
(define (eLeft? either) (and (pair? either) (not (car either))))
(define fromLeftRight cdr)

; ***************
; **** STATE ****
; ***************

; an empty stack
(define mkEmptyStack (lambda () '()))

; an empty environment
(define mkEmptyEnv (lambda () (cons '() '())))

; an interpreter state is just (stack . env)
(define mkEmptyState (lambda () (cons (mkEmptyStack) (mkEmptyEnv))))

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
; cannot return an error
(define (stStackUpd f state)
  (let ((fValue (f (stGetStack state))))
    (stUpdateStack state fValue)
    (eRight '())))

; push onto stack
(define (stackPush var)
  (lambda (stack) (cons var stack)))
(define (stStackPush state var) 
  (stStackUpd (stackPush var) state))

; stack depth
(define depth length)
(define (stStackDepth state) (depth (stGetStack state)))

; pop off stack
; when popping, return (cons <popped value> <new stack>)
; since we represent the stack as a list, pop is just
; the identity function
(define (stackPop stack)
  (if (> (depth stack) 0)
    (cons (eRight (car stack)) (cdr stack))
    (cons (eLeft "pop: stack empty") stack)))
(define (stStackPop state) (stStackUpd2 stackPop state))

; swap the 0th and 1st elements of the stack
(define (stackSwap stack)
  (if (> (depth stack) 1)
    (cons (eRight '()) (cons (cadr stack) (cons (car stack) (cddr stack))))
    (cons (eLeft "swap: not enough elements") stack)))
(define (stStackSwap state) (stStackUpd2 stackSwap state))

; drop the 0th element
(define (stackDrop stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cdr stack))
    (cons (eLeft "drop: stack empty") stack)))
(define (stStackDrop state) (stStackUpd2 stackDrop state))

; clear the stack, i.e., replace it with emptyState
(define stackClear (lambda (x) (mkEmptyStack)))
(define (stStackClear state) (stStackUpd stackClear state))

; a generalized stack operation that takes the 0th elem
; off the stack, expecting an integer, and produces a
; modified stack as a result
(define (stackOpN f)
  (lambda (stack)
    (let* ((popVal (stackPop stack))
           (popEither (car popVal))
           (popRes (fromLeftRight popEither))
           (popRem (cdr popVal)))
      (cond ((eLeft? popEither) popVal)
            ((number? popRes)
             (if (<= (number->int popRes) (depth popRem))
               (cons (eRight '()) (f (number->int popRes) popRem))
               (cons (eLeft "stackOpN: not enough elements") stack)))
            (else (cons (eLeft "stackOpN: non-numeric argument") stack))))))

; drop N elements after the 0th
(define stackDropN
  (stackOpN (lambda (n st) (iterateNOrNull cdr n st))))
(define (stStackDropN state) (stStackUpd2 stackDropN state))

; roll the top N elements after the 0th
(define stackRollN (stackOpN rollN))
(define (stStackRollN state) (stStackUpd2 stackRollN state))

; unroll the top N elements after the 0th
(define stackUnrollN (stackOpN unrollN))
(define (stStackUnrollN state) (stStackUpd2 stackUnrollN state))

; dup the top element
(define (stackDup stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cons (car stack) stack))
    (cons (eLeft "dup: stack empty") stack)))
(define (stStackDup state) (stStackUpd2 stackDup state))

; dup the first N elements after the 0th
(define stackDupN (stackOpN dupN))
(define (stStackDupN state) (stStackUpd2 stackDupN state))

; duplicate the second element on the stack
(define (stackOver stack)
  (if (> (depth stack) 1)
    (cons (eRight '()) (cons (cadr stack) stack))
    (cons (eLeft "over: not enough elements") stack)))
(define (stStackOver state) (stStackUpd2 stackOver state))

; duplicate the nth element on the stack
(define stackPickN (stackOpN pickN))
(define (stStackPickN state) (stStackUpd2 stackPickN state))

; generalized stack operation that takes the 0th elem
; off the stack and evaluates it for truth. If true,
; a supplied stackop is executed as long as there are
; sufficient elements in the stack
(define (stackOpBool bool f)
  (lambda (stack)
    (let* ((popVal (stackPop stack))
           (popEither (car popVal))
           (popRes (fromLeftRight popEither))
           (popRem (cdr popVal)))
      (cond ((eLeft? popEither) popVal)
            ((not (boolean? popRes)) (cons (eLeft "stackOpBool: not a boolean") stack))
            ((eq? bool popRes) (f popRem))
            (else (cons (eRight '()) popRem))))))

; swapIf
(define stackSwapIf
  (stackOpBool #t stackSwap))
(define (stStackSwapIf state) (stStackUpd2 stackSwapIf state))

; swapUnless
(define stackSwapUnless
  (stackOpBool #f stackSwap))
(define (stStackSwapUnless state) (stStackUpd2 stackSwapUnless state))

; dropIf
(define stackDropIf
  (stackOpBool #t stackDrop))
(define (stStackDropIf state) (stStackUpd2 stackDropIf state))

; dropUnless
(define stackDropUnless
  (stackOpBool #f stackDrop))
(define (stStackDropUnless state) (stStackUpd2 stackDropUnless state))

; ###############
; #### TESTS ####
; ###############

(define myState (mkEmptyState))
(stStackPush myState 1)
(stStackPush myState 2)
(stStackPush myState 3)
(stStackPush myState 4)
(stStackPush myState 5)
(stStackPush myState 2)
(display myState) (newline)

