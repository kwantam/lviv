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

; ***********************
; **** SPECIAL FORMS ****
; ***********************
; most of these functions handle special forms
; like lambda, let, define, et cetera
; if is in here, but it's really syntactic sugar

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

; turn a thunk back into its code
(define (stUnThunk state)
  (let* ((fnLArg (stStackPop state))
         (fnArg (fromLeftRight fnLArg)))
    (cond ((eLeft? fnLArg) fnLArg)
          ((thunkElm? fnArg)
           (stStackPush state (thunkElm->elm fnArg)))
          (else
            (rewind state (list fnArg) "not a thunk")))))

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

; take the code and args from a lambda
; and put them on a stack
(define (stUnLambda state)
  (let* ((fnLArg (stStackPop state))
         (fnArg (fromLeftRight fnLArg)))
    (cond ((eLeft? fnLArg) fnLArg)
          ((lambda? fnArg)
           (stStackPush state (lambda-code fnArg))
           (stStackPush state (lambda-args fnArg)))
          (else
            (rewind state (list fnArg) "not a lambda")))))

; create a primitive binding.
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
             (eRight (applyMap (force fnState)
                      (car (force fnCodeParts))))))
         (fnResult (delay (with-exception-catcher           ; wrap above with exception handler
                            exceptionHandlerQuiet
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
                                     (applyMap (force fnState)
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

