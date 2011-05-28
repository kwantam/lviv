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
;

; ***************
; **** STACK ****
; ***************
; operations that work directly on the stack, generally
; constituting or used by the lviv primitive stack operations

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
(define (stStackDepth state) (stStackPush state (depth (stGetStack state))))

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
    (if (< (depth stack) n)
      (cons (eLeft "popN: not enough arguments") stack)
      (letrec ((sPopNHlp
                 (lambda (stack accum n)
                   (if (= n 0)
                     (cons (eRight accum) stack)
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
      (with-exception-catcher
        (lambda (x) (cons (eLeft "type error") stack))
        (lambda ()
          (cond ((eLeft? popEither) popVal)
                ((bool popRes) (f popRem))
                (else (cons (eRight '()) popRem))))))))

; swapIf
(define stackSwapIf
  (stackOpBool =true? stackSwap))
(define (stStackSwapIf state) (stStackUpd2 stackSwapIf state))

; swapUnless
(define stackSwapUnless
  (stackOpBool =false? stackSwap))
(define (stStackSwapUnless state) (stStackUpd2 stackSwapUnless state))

; dropIf
(define stackDropIf
  (stackOpBool =true? stackDrop))
(define (stStackDropIf state) (stStackUpd2 stackDropIf state))

; dropUnless
(define stackDropUnless
  (stackOpBool =false? stackDrop))
(define (stStackDropUnless state) (stStackUpd2 stackDropUnless state))

; uncons
; if the first element of the stack is a list, replace it
; with car and cdr of list in 0th and 1st positions, respectively
(define (stackUncons stack)
  (let* ((popVal (stackPop stack))
         (popEither (car popVal))
         (popRes (fromLeftRight popEither))
         (popRem (cdr popVal)))
    (cond ((eLeft? popEither) popVal)
          ((not (pair? popRes)) (cons (eLeft "not a pair") stack))
          (else (cons (eRight '()) 
                      (append (list (car popRes) (cdr popRes))
                              popRem))))))
(define (stStackUncons state) (stStackUpd2 stackUncons state))

; a thunk is a piece of code that is idempotent through eval
; and requires an apply to get "opened up"
; applying a thunk is like evaling its contents
(define (stackThunk stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cons (mkThunkElm (car stack)) (cdr stack)))
    (cons (eLeft "thunk: stack empty"))))
(define (stStackThunk state) (stStackUpd2 stackThunk state))

; if
; <consequent> <alternative> <bool> if
(define (stIf state)
  (begin (stStackSwapUnless state)
         (stStackDrop state)
         (stStackThunk state)
         (stApply state)))

; unless
; <consequent> <alternative> <bool> if
(define (stUnless state)
  (begin (stStackSwapIf state)
         (stStackDrop state)
         (stStackThunk state)
         (stApply state)))

; nop does nothing
(define (stNop state) (eRight '()))

; env prints out the environment
(define (stEnv state)
  (begin (lviv-ppenv (stGetEnv state))
         (eRight '())))

; turn some code and a binding list into a lambda
; put it on the stack
(define (stLambda state)
  (let* ((fnLArgs (stStackNPop state 2))
         (fnArgs (delay (cadr (fromLeftRight fnLArgs))))
         (fnxCode (delay (car (fromLeftRight fnLArgs))))
         (fnCode 
           (delay
             (if (list? (force fnxCode))
               (force fnxCode)
               (list (force fnxCode)))))
         (fnLambda
           (delay (mkLambda (force fnCode)
                            (force fnArgs)
                            (stGetEnv state)))))
    (cond ((eLeft? fnLArgs) fnLArgs) ; popN failed
          ((not (list? (force fnArgs)))
           (rewind state (reverse (fromLeftRight fnLArgs))
                   "invalid arglist supplied"))
          ((not (allWith quote-symbol-elm? (force fnArgs)))
           (rewind state (reverse (fromLeftRight fnLArgs))
                   "arglist must be quoted symbols"))
          (else
            (eRight (stStackPush state (force fnLambda)))))))

; as above, but creates a primitive binding.
(define (stPrimitive state)
  (let* ((fnLArgs (stStackNPop state 2))
         (fnId (delay (cadr (fromLeftRight fnLArgs))))
         (fnArity (delay (car (fromLeftRight fnLArgs))))
         (fnBinding (delay (mkPrimBinding (force fnId)
                                          (force fnArity)))))
    (cond ((eLeft? fnLArgs) fnLArgs)
          (else (eRight (stStackPush state 
                                     (force fnBinding)))))))

; `let` makes a temporary environment and executes a thunk in it
; <code> <bindings> let
; <bindings> is a list of name-value pairs
; values are executed in sequence as thunks, and after
; execution the top value on the stack is popped and
; bound to the given name in the temporary environment
(define (stLet state)
  (let* ((fnLArgs (stStackNPop state 2))                    ; args for the let
         (fnBinds (delay (cadr (fromLeftRight fnLArgs))))   ; let-bindings
         (fnxCode (delay (car (fromLeftRight fnLArgs))))    ; let-code
         (fnCode                                            ; make sure let-code is a list
           (delay
             (if (list? (force fnxCode))
               (force fnxCode)
               (list (force fnxCode)))))
         (fnCodeParts (delay (splitAt (- (length (force fnCode)) 1) ; split code for
                                      (force fnCode))))     ; tail call optimization
         (fnState (delay (cons (stGetStackBox state)    ; make a new state for the let
                               (envNewChild (stGetEnv state)))))
         (fnCompResult                                      ; run the first part of the code
           (lambda ()
             (eRight ((applyMap (force fnState))
                      (car (force fnCodeParts))))))
         (fnResult (delay (with-exception-catcher           ; wrap above with exception handler
                            (exceptionHandler #f)
                            fnCompResult)))
         (fnLastEval                                        ; last eval happens inside let environ
           (delay (lviv-eval (force fnState) (cadr (force fnCodeParts))))))
    (cond ((eLeft? fnLArgs) fnLArgs)                        ; popN failed
          ((not (list? (force fnBinds)))                    ; invalid bindings - not a list
           (rewind state (reverse (fromLeftRight fnLArgs))
                   "invalid bindings supplied"))
          ((not (allWith                                    ; invalid bindings - not assoc list
                  (lambda (x)
                    (and (pair? x)
                         (quote-symbol-elm? (car x))))
                  (force fnBinds)))
           (rewind state (reverse (fromLeftRight fnLArgs))
                   "binding list must be pairs of (sym . binding)"))
          ((not (allWith                                    ; run the bindings
                  (lambda (x)
                    (let ((bindVal (delay (stStackPop (force fnState))))
                          (bindCode 
                            (allWith eRight?
                                     ((applyMap (force fnState))
                                      (if (list? (cdr x))
                                        (cdr x)
                                        (list (cdr x)))))))
                      (and bindCode
                           (eRight? (force bindVal))
                           (stEnvAddBinding
                             (force fnState)
                             (cons (car x)
                                   (fromLeftRight (force bindVal)))))))
                  (force fnBinds)))
           (rewind state (reverse (fromLeftRight fnLArgs))
                   "bindings failed"))
          ((eLeft? (force fnResult))                        ; run computation, rewind if exception
           (rewind state
                   (reverse (fromLeftRight fnLArgs))
                   (fromLeftRight (force fnResult))))
          ((eLeft? (force fnLastEval))                      ; check that last eval was successful
           (rewind state                                    ; otherwise rewind and throw error
                   (reverse (fromLeftRight fnLArgs))
                   (fromLeftRight (force fnLastEval))))
          ; this is an "else" since stUpdateStack always returns a true value
          ; update the stack with whatever new values from the let, then run
          ; the final computation
          (else
            (lviv-apply (force fnState) (force fnLastEval))))))

; temporary stack
(define (stTStk state)
  (eRight (stUpdateStackBox state (cons '() (stGetStackBox state)))))

(define (stUnTStk return?)
  (lambda (state)
    (cond ((< (length (stGetStackBox state)) 2)
           (eLeft "already in outermost stack"))
          (return?
            (let ((toPush (stStackPop state)))
              (if (eLeft? toPush)
                toPush
                (begin (stUpdateStackBox
                         state (cdr (stGetStackBox state)))
                       (eRight (stStackPush
                                 state
                                 (fromLeftRight toPush)))))))
          (else
            (eRight 
              (stUpdateStackBox 
                state (cdr (stGetStackBox state))))))))

(define (stPrintStack state)
  (lviv-ppstack (stGetStack state)
                (stEnvLookupBinding
                  state
                  '_stack_display_depth)))

; bind a stackop function to a symbol,
; make a test that returns the function when
; given the symbol
(define (stackOp? symb f)
  (lambda (op) (if (eq? symb op) f #f)))

; stackop lookup
(define (stStackOp? op)
  (or ((stackOp? 'depth stStackDepth) op)
      ((stackOp? 'swap stStackSwap) op)
      ((stackOp? 'drop stStackDrop) op)
      ((stackOp? 'clear stStackClear) op)
      ((stackOp? 'dropN stStackDropN) op)
      ((stackOp? 'roll stStackRollN) op)
      ((stackOp? 'unroll stStackUnrollN) op)
      ((stackOp? 'dup stStackDup) op)
      ((stackOp? 'dupN stStackDupN) op)
      ((stackOp? 'over stStackOver) op)
      ((stackOp? 'pick stStackPickN) op)
      ((stackOp? 'swapIf stStackSwapIf) op)
      ((stackOp? 'swapUnless stStackSwapUnless) op)
      ((stackOp? 'dropIf stStackDropIf) op)
      ((stackOp? 'dropUnless stStackDropUnless) op)
      ((stackOp? 'uncons stStackUncons) op)
      ((stackOp? 'define stDefine) op)
      ((stackOp? 'undefLocal (stUndef #t)) op)
      ((stackOp? 'undef (stUndef #f)) op)
      ((stackOp? 'eval stEval) op)
      ((stackOp? 'apply stApply) op)
      ((stackOp? 'thunk stStackThunk) op)
      ((stackOp? 'lambda stLambda) op)
      ((stackOp? 'primitive stPrimitive) op)
      ((stackOp? 'if stIf) op)
      ((stackOp? 'unless stUnless) op)
      ((stackOp? 'env stEnv) op)
      ((stackOp? 'nop stNop) op)
      ((stackOp? 'let stLet) op)
      ((stackOp? 'tstk stTStk) op)
      ((stackOp? 'untstk (stUnTStk #f)) op)
      ((stackOp? 'rtstk (stUnTStk #t)) op)
      ((stackOp? 'stk stPrintStack) op)
      ))

