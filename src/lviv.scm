;
;Copyright (c) 2011 Riad S. Wahby <rsw@jfet.org>
;
;Permission is hereby granted, free of charge, to any person obtaining a copy
;of this software and associated documentation files (the "Software"), to deal
;in the Software without restriction, including without limitation the rights
;to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;copies of the Software, and to permit persons to whom the Software is
;furnished to do so, subject to the following conditions:
;
;The above copyright notice and this permission notice shall be included in
;all copies or substantial portions of the Software.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;THE SOFTWARE.
;
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

; update environment
(define (stUpdateEnv oldState newEnv) (set-cdr! oldState newEnv))
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

; push a list of values back onto the stack
(define (stackNPush var)
  (lambda (stack) (append var stack)))
(define (stStackNPush state var)
  (stStackUpd (stackNPush var) state))

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
    (cons (eLeft "pop: stack empty") '())))

(define (stStackPop state) (stStackUpd2 stackPop state))

; pop n items off stack
; this is used when executing functions
(define (stackPopN n)
  (lambda (stack)
    (if (<= (depth stack) n)
      (cons (eLeft "popN: not enough arguments") '())
      (letrec ((sPopNHlp
                 (lambda (stack accum n)
                   (if (= n 0)
                     (cons (eRight (reverse accum)) stack)
                     (sPopNHlp (cdr stack) (cons (car stack) accum) (- n 1))))))
        (sPopNHlp stack '() n)))))
(define (stStackNPop state n) (stStackUpd2 (stackPopN n) state))

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
    (cons (eLeft "drop: stack empty") '())))
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
    (cons (eLeft "dup: stack empty") '())))
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

; #############
; #### ENV #### 
; #############

; env bindings
(define envBindings car)
(define envFirstBinding caar)
(define envRemBindings cdar)
(define envSetBindings set-car!)
(define firstBinding car)
(define restBindings cdr)
(define rRestBindings cddr)
(define setRestBindings set-cdr!)
; get env parent
(define envParent cdr)
; no updateParent necessary, we never rebase an env

; is this the global env?
(define (stGlobalEnv? state) (null? (envParent (stGetEnv state))))

; make a new child env from the parent
(define (envNewChild env) (cons '() env))
(define (stEnvNewChild state) (stUpdateEnv state (envNewChild (stGetEnv state))))

(define (stEnvParent state)
  (if (not (stGlobalEnv? state))
    (stUpdateEnv state (envParent (stGetEnv state)))))

; convert an envBinding operation to a stEnvBinding operation
(define (stEnvBindOp f)
  (lambda (state item) (f (stGetEnv state) item)))

; delete binding from env
(define (envDelBinding env name)
  (letrec ((delHlp
             (lambda (bindings)
               (cond ((null? (restBindings bindings)) #f)
                     ((eq? name (car (firstBinding (restBindings bindings))))
                      (setRestBindings bindings (rRestBindings bindings)))
                     (else (delHlp (restBindings bindings)))))))
    (cond ((null? (envBindings env)) #f)
          ((eq? name (car (firstBinding (envBindings env))))
           (envSetBindings env (restBindings (envBindings env))))
          (else (delHlp (envBindings env))))))
(define stEnvDelBinding (stEnvBindOp envDelBinding))

; insert an item into the environment
; does not check whether item is already there
(define (envAddBinding env item)
  (envSetBindings env (cons item (envBindings env))))

; update or insert binding into env
(define (envUpdateBinding env item)
  (letrec ((updateHlp
             (lambda (bindings) 
               (cond ((null? bindings) (envAddBinding env item))
                     ((eq? (car item) (car (firstBinding bindings)))
                      (set-cdr! (firstBinding bindings) (cdr item)))
                     (else (updateHlp (restBindings bindings)))))))
    (updateHlp (envBindings env))))
(define stEnvUpdateBinding (stEnvBindOp envUpdateBinding))

(define (envLookupBinding env name)
  (if (null? env)
    (eLeft "not found")
    (letrec ((lookupHlp
               (lambda (bindings)
                 (cond ((null? bindings) (envLookupBinding (envParent env) name))
                       ((eq? name (car (firstBinding bindings)))
                        (eRight (cdr (firstBinding bindings))))
                       (else (lookupHlp (restBindings bindings)))))))
      (lookupHlp (envBindings env)))))
(define stEnvLookupBinding (stEnvBindOp envLookupBinding))

; ###########################
; #### EXCEPTION HANDLER ####
; ###########################
; we want to provide reasonable exceptions to the user, so we do our best
; to catch what's coming from the interpreter and turn it into something
; intelligible
(define (exceptionHandler exc)
  (cond ((noncontinuable-exception? exc) (eLeft (noncontinuable-exception-reason exc)))
        ((heap-overflow-exception? exc) (eLeft "heap overflow"))
        ((stack-overflow-exception? exc) (eLeft "call stack overflow"))
        ((os-exception? exc) (eLeft (os-exception-message exc)))
        ((no-such-file-or-directory-exception? exc) (eLeft "no such file or directory"))
        ((unbound-os-environment-variable-exception? exc) (eLeft "unbound env variable"))
        ((scheduler-exception? exc) (eLeft "scheduler exception"))
        ((deadlock-exception? exc) (eLeft "deadlock exception"))
        ((abandoned-mutex-exception? exc) (eLeft "abandoned mutex"))
        ((join-timeout-exception? exc) (eLeft "join timeout"))
        ((started-thread-exception? exc) (eLeft "thread started"))
        ((terminated-thread-exception? exc) (eLeft "thread terminated"))
        ((uncaught-exception? exc) (eLeft "uncaught exception"))
        ((cfun-conversion-exception? exc) (eLeft "C function exception"))
        ((sfun-conversion-exception? exc) (eLeft "Sfun exception"))
        ((multiple-c-return-exception? exc) (eLeft "multiple C return"))
        ((datum-parsing-exception? exc) (eLeft "bad read"))
        ((expression-parsing-exception? exc) (eLeft "bad parse"))
        ((unbound-global-exception? exc) (eLeft "unbound global exception"))
        ((type-exception? exc) (eLeft "type exception"))
        ((range-exception? exc) (eLeft "range exception"))
        ((improper-length-list-exception? exc) (eLeft "improper length list"))
        ((wrong-number-of-arguments-exception? exc) (eLeft "wrong number of arguments"))
        ((number-of-arguments-limit-exception? exc) (eLeft "number of arguments limit"))
        ((nonprocedure-operator-exception? exc) (eLeft "nonprocedure operator"))
        ((unknown-keyword-argument-exception? exc) (eLeft "unknown keyword argument"))
        ((keyword-expected-exception? exc) (eLeft "keyword expected"))
        ((error-exception? exc) (eLeft (string-append "error: " (error-exception-message exc))))
        ((divide-by-zero-exception? exc) (eLeft "divide by zero"))
        (else (eLeft "unknown exception"))))

; ########################
; #### FUNCTION CALLS ####
; ########################

; make a primitive binding to stick in the env
(define (mkPrimBinding id arity)
  (list 'primitive arity id))

; primitives
(define primitive-arity cadr)
(define primitive-id caddr)
(define (primitive? obj)
  (and (list? obj) (eq? (car obj) 'primitive) (= (length obj) 3)))

; primitive call
(define (stPrimCall state)
  (let* ((fName (stStackPop state))
         (fBinding (delay (stEnvLookupBinding state (fromLeftRight fName))))
         (fnNArgs (delay (primitive-arity (fromLeftRight (force fBinding)))))
         (fnArgs (delay (stStackNPop state (force fnNArgs))))
         (fnCompResult
           (lambda ()
             (eRight (apply (eval (primitive-id (fromLeftRight (force fBinding))))
                            (fromLeftRight (force fnArgs))))))
         (fnResult (delay (with-exception-catcher exceptionHandler fnCompResult))))
    (cond ((eLeft? fName) fName)
          ((eLeft? (force fBinding)) 
           (begin (stStackPush state (fromLeftRight fName))
                  (force fBinding)))
          ((not (primitive? (fromLeftRight (force fBinding))))
           (begin (stStackPush state (fromLeftRight fName))
                  (eLeft "not a primitive procedure")))
          ((eLeft? (force fnArgs)) (force fnArgs))
          ((eLeft? (force fnResult))
           (begin (stStackNPush state (fromLeftRight (force fnArgs)))
                  (force fnResult)))
          (else (stStackPush state (fromLeftRight (force fnResult)))))))

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

(stEnvUpdateBinding myState '(a 1))
(stEnvUpdateBinding myState '(b 2))
(stEnvUpdateBinding myState '(c 3))
(stEnvUpdateBinding myState '(d 4))
(stEnvUpdateBinding myState '(e 5))
(stEnvUpdateBinding myState '(a 6))
(stEnvDelBinding myState 'd)

(stEnvNewChild myState)
(stEnvUpdateBinding myState '(f 100))
(stEnvUpdateBinding myState '(b 200))

(display (stEnvLookupBinding myState 'c)) (newline)
(display (stEnvLookupBinding myState 'd)) (newline)
(display (stEnvLookupBinding myState 'b)) (newline)

(display myState) (newline)
(stEnvParent myState)
(stEnvParent myState)
(stEnvParent myState)
(stEnvParent myState)
(display myState) (newline)

(stEnvNewChild myState)
(stEnvUpdateBinding myState '(f 100))
(stEnvUpdateBinding myState '(b 200))
(stEnvUpdateBinding myState (cons '+ (mkPrimBinding '+ 2)))
(stEnvUpdateBinding myState (cons '- (mkPrimBinding '- 2)))
(stEnvUpdateBinding myState (cons 'cons (mkPrimBinding 'cons 2)))
(stEnvUpdateBinding myState (cons 'car (mkPrimBinding 'car 1)))
(stEnvUpdateBinding myState (cons 'cdr (mkPrimBinding 'cdr 1)))

(stStackPush myState '+)

(display myState) (newline)

