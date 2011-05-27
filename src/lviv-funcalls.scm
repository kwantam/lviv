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

; ########################
; #### FUNCTION CALLS ####
; ########################
; all the function calling machinery
; some of the symbol defs in here should
; probably move to lviv-symbols.scm

(define (stLambdaCall state binding)
  (if (null? (lambda-code binding)) (eRight '())
  (let* ((rfunc (if (lambda-reverse? binding) values reverse))
         (fnArgNames (lambda-args binding))
         (fnNArgs (length fnArgNames))
         (fnArgs (delay (stStackNPop state fnNArgs)))
         (lambdaCodeParts                                   ; tail call optimization
           (splitAt (- (length (lambda-code binding)) 1)    ; take the last call in the lambda
                    (lambda-code binding)))                 ; apply it in tail position
         (lambdaState                                       ; state during the lambda
           (delay 
             (cons (stGetStackBox state) 
                   (cons (zip fnArgNames (rfunc (fromLeftRight (force fnArgs))))
                         (lambda-env binding)))))
         (fnCompResult                                      ; apply all but last piece of code
           (lambda ()
             (eRight ((applyMap (force lambdaState))
                      (car lambdaCodeParts)))))
         (fnResult (delay (with-exception-catcher           ; catch errors in above
                            (exceptionHandler #f)
                            fnCompResult)))
         (fnFinalEval                                       ; eval last piece in lambda env
           (delay (lviv-eval (force lambdaState) (cadr lambdaCodeParts)))))
    (cond ((eLeft? (force fnArgs)) (force fnArgs))
          ((eLeft? (force fnResult))
           (rewind state
                   (reverse (fromLeftRight (force fnArgs)))
                   (fromLeftRight (force fnResult))))
          ((eLeft? (force fnFinalEval))                     ; make sure final eval works
           (rewind state                                    ; otherwise rewind and throw err
                   (reverse (fromLeftRight (force fnArgs)))
                   (fromLeftRight (force fnFinalEval))))
          ; this is like an "else", since stUpdateStack always returns a true value
          ; so we first update the stack, then we call the already evaluated call
          ; from the lambda in the original state, which gets the last call into
          ; tail position
          (else
            (lviv-apply (force lambdaState) (force fnFinalEval))))))) ; tail call

; primitive call
; no tail call optimization necessary here; Scheme will do it
; for calls that require it, and to us it's just one monolithic
; call
(define (stPrimCall state binding)
  (let* ((rfunc (if (primitive-reverse? binding) values reverse))
         (fnNArgs (delay (primitive-arity binding)))
         (fnArgs (delay (stStackNPop state (force fnNArgs))))
         (fnCompResult
           (lambda ()
             (eRight (apply (eval (primitive-id binding))
                            (rfunc (fromLeftRight (force fnArgs)))))))
         (fnResult (delay (with-exception-catcher 
                            (exceptionHandler #f)
                            fnCompResult))))
    (cond ((eLeft? (force fnArgs)) (force fnArgs))
            ; if there aren't enough args, the procedure fails
            ; and the stack doesn't get rewound any further
            ; note that if stStackNPop fails, it will rewind what
            ; it did
          ((eLeft? (force fnResult))
           (rewind state
                   (reverse (fromLeftRight (force fnArgs)))
                   (fromLeftRight (force fnResult))))
            ; if the primitive application fails, put the args
            ; back on the stack
          (else (stStackPush state (fromLeftRight (force fnResult)))))))
            ; else push the new value onto the stack

