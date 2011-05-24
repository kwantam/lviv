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
  (let* ((rfunc (if (lambda-reverse? binding) values reverse))
         (fnArgNames (lambda-args binding))
         (fnNArgs (length fnArgNames))
         (fnArgs (delay (stStackNPop state fnNArgs)))
         (lambdaState
           (delay (cons (stGetStack state)
                        (cons (zip fnArgNames (rfunc (fromLeftRight (force fnArgs))))
                              (lambda-env binding)))))
         (fnCompResult
           (lambda ()
             (eRight ((applyMap (force lambdaState))
                      (lambda-code binding)))))
         (fnResult (delay (with-exception-catcher
                            (exceptionHandler #f)
                            fnCompResult))))
    (cond ((eLeft? (force fnArgs)) (force fnArgs))
          ((eLeft? (force fnResult))
           (rewind state
                   (reverse (fromLeftRight (force fnArgs)))
                   (fromLeftRight (force fnResult))))
          (else (begin (stUpdateStack
                         state
                         (stGetStack (force lambdaState)))
                       (force fnResult))))))

; primitive call
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


