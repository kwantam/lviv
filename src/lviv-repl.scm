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

; ###############
; #### EVAL #####
; ###############
; repl

; pop off an elm from the stack and eval it
(define (stEval state)
  (let* ((fnEArg (stStackPop state))
         (fnArg (fromLeftRight fnEArg)))
    (if (eLeft? fnEArg)
      fnEArg
      (stStackPush state (lviv-eval state fnArg)))))

; eval->apply on a list
; this is used to evaluate keyboard input and for thunks
(define (applyMap state elm)
  (map (lambda (x) 
         (lviv-apply state (lviv-eval state x)))
       elm))

; since lviv-apply always works on AST, stApply and lviv-apply are
; basically identical, but we have to wrap a safe pop around the
; former to get the latter
; thunks are treated specially by stApply; it unwraps and eval->applies them
(define (stApply state) ; 
  (let* ((fnEArg (stStackPop state))            ; pop off element
         (fnArg (fromLeftRight fnEArg)))
    (cond ((eLeft? fnEArg) fnEArg)              ; pop unsuccessful?
          ((thunkElm? fnArg)                    ; thunk?
           (if (null? (thunkElm->elm fnArg)) (eRight '()) ; do nothing with null thunk
           (let* ((thunkCode (thunkElm->elm fnArg))       ; else perform tail call opt
                  (thunkCodeParts (splitAt (- (length thunkCode) 1)
                                           thunkCode)))
             (begin (applyMap state (car thunkCodeParts)) ; call first part of thunk
                    (lviv-apply                             ; then tail call last part
                      state
                      (lviv-eval state
                                 (cadr thunkCodeParts)))))))
          (else (lviv-apply state fnArg)))))    ; otherwise, just apply it like anything else

; mapping of stackop to procedure
(define stackOpMapping
  (list `(depth . ,stStackDepth)
        `(swap . ,stStackSwap)
        `(drop . ,stStackDrop)
        `(clear . ,stStackClear)
        `(dropN . ,stStackDropN)
        `(roll . ,stStackRollN)
        `(unroll . ,stStackUnrollN)
        `(dup . ,stStackDup)
        `(dupN . ,stStackDupN)
        `(over . ,stStackOver)
        `(pick . ,stStackPickN)
        `(swapIf . ,stStackSwapIf)
        `(swapUnless . ,stStackSwapUnless)
        `(dropIf . ,stStackDropIf)
        `(dropUnless . ,stStackDropUnless)
        `(uncons . ,stStackUncons)
        `(define . ,stDefine)
        `(undefLocal . ,(stUndef #t))
        `(undef . ,(stUndef #f))
        `(eval . ,stEval)
        `(apply . ,stApply)
        `(thunk . ,stStackThunk)
        `(unthunk . ,stUnThunk)
        `(lambda . ,stLambda)
        `(unlambda . ,stUnLambda)
        `(primitive . ,stPrimitive)
        `(if . ,stIf)
        `(unless . ,stUnless)
        `(env . ,stEnv)
        `(nop . ,stNop)
        `(let . ,stLet)
        `(tstk . ,stTStk)
        `(dtstk . ,stDTStk)
        `(untstk . ,(stUnTStk #f))
        `(rtstk . ,(stUnTStk #t))
        `(stk . ,stPrintStack)
        `(lstk . ,stStackToList)
        `(unlstk . ,stListToStack)
        ))

; list of stackops; doing this now lets us not
; call car for every lookup in the list above
(define stackOpListing (map car stackOpMapping))

; lookup item in association list
(define (carLookup symb lst)
  (if (eq? symb (caar lst))
    (cdar lst)
    (carLookup symb (cdr lst))))
    

; the main eval procedure
(define (lviv-eval state item)
  (let ((lookupElm (lambda (env name)
                     (let ((lkRef (envLookupBinding env name)))
                       (if (eLeft? lkRef)
                         (symbErr name "lookup failed")
                         (fromLeftRight lkRef))))))
    (cond ((symbol? item)
           (cond ((member item stackOpListing)            ; stackop?
                  (mkStackOpElm                           ; make stackop element
                    (carLookup item stackOpMapping)
                    item))
                 ((static-symbol-unchecked? item)                 ; &foo -> (& foo env)
                  (mkStaticSymbolElm item (stGetEnv state)))
                 ((quote-symbol-unchecked? item)                  ; *bar -> bar
                  (mkQuoteSymbolElm item))
                 ((reverse-symbol-unchecked? item)                ; :cons -> cons in reverse
                  (let ((iBind (lookupElm (stGetEnv state)
                                          (reverse-symbol->symbol item)))) ; look it up
                    (cond ((primitive? iBind) (prim-reverse iBind))        ; reverse if possible
                          ((lambda? iBind) (lambda-reverse iBind))         ; "
                          (else                                            ; otherwise, error
                            (symbErr (reverse-symbol->symbol item)
                                     "can only reverse lambda or primitive")))))
                 (else                                            ; otherwise
                   (lookupElm (stGetEnv state)
                              item))))
          ((static-symbol-elm? item)
           (lookupElm (static-symbol-env item)       ; static symbol
                      (static-symbol-sym item)))     ; resolve in attached env
          ((lviv-tagged? item) item)                 ; all other lviv-tagged items are idempotent
          ((and (list? item) (pair? item))                ; has to be list and not nil
           (map (lambda (x) (lviv-eval state x)) item))   ; eval the contents
          ((pair? item)                                   ; a pair gets car and cdr evaluated
           (cons (lviv-eval state (car item))
                 (lviv-eval state (cdr item))))
          (else item))))                                  ; otherwise, I guess it's idempotent

; apply the output from eval to the stack
(define (lviv-apply state item)
  ((lambda (result) (if (eLeft? result) (stackError result) result))
   (cond ((eLeft? item) item)
         ((elm? item)
          (cond ((stackOpElm-unchecked? item)           ; stackop gets executed
                 ((stackOpElm->stackop item) state))
                ((primitive-unchecked? item)            ; primitive gets called
                 (stPrimCall state item))
                ((lambda-unchecked? item)               ; lambda gets called
                 (stLambdaCall state item))
                (else                                   ; other elms are idempotent
                  (stStackPush state item))))
         (else (stStackPush state item))))) ; else just push it on the stack

; slurp in input by repeatedly reading until there's no more
; to be had. We (read-all) does this for us, but we have to
; set the timeout to zero so the user doesn't have to ^D
(define (read-list . port)
  (let* ((port (if (null? port) (current-input-port) (car port)))
         (toSet0 (input-port-timeout-set! port 0))
         (readRes (read-all))
         (toSetf (input-port-timeout-set! port #f)))
    readRes))

; we use read-list rather than read so that we get all of the inputs at once,
; and don't end up printing the state of the stack between each element in
; the input as we apply them
(define (lviv-repl state input) ; repl
  (cond ((equal? input '(#!eof)) #f)
        (else
          (let ((allInput (append input (read-list))))
            (with-exception-catcher exceptionHandlerPrint
                                    (lambda () (applyMap state allInput))))
          (stPrintStack state)
          (display "> ")
          (lviv-repl state (list (read))))))

; read in a file as if entered at the repl
(define (lviv-file state file)
  (let* ((fInput (with-exception-catcher
                   exceptionHandlerQuiet
                   (lambda () (open-input-file file))))
         (fRead (delay (read-all fInput))))
    (if (eLeft? fInput)
      fInput
      (begin (with-exception-catcher
               exceptionHandlerPrint
               (lambda () (applyMap state (force fRead))))
             (stPrintStack state)))))

; default stack display depth
(define _stack_display_depth 10)

; engineering notation
(define (eng num)
  (let ((x (or (and (number? num) num) (string->number num))))
    (cond 
      ((not x) (raise "type exception"))
      ((and (not (real? x)) (complex? x))
       (string-append 
         (eng (real-part x))
         (if (< (psign (imag-part x)) 0) "-" "+")
         (eng (imag-part x))
         "i"))
      (else
        (let* ((xxpon (xpon x))
               (x3pon (* 3 
                         (- (quotient xxpon 3)
                            (if (and
                                  (< xxpon 0)
                                  (not (zero? (remainder xxpon 3))))
                              1 0))))
               (xmant (/ x (expt 10 x3pon))))
          (string-append (number->string 
                           (exact->inexact (rnd xmant 12)))
                         "e"
                         (number->string x3pon)))))))

; prettyprint for lviv elements
(define (lviv-pp newline? ugly?)
  (lambda (elm)
    (let ((newln (lambda () (if newline? (display "\n") (display " ")))))
      (cond ((static-symbol-elm? elm)
             (begin (display "&")
                    (display (static-symbol-sym elm))
                    (and ugly? (begin (display "(env#")
                                        (display (static-symbol-sn elm))
                                        (display ")")))
                    (newln)))
            ((stackOpElm? elm)
             (begin (and ugly? (display "#<stackOp "))
                    (display (stackOpElm-sym elm))
                    (and ugly? (display ">"))
                    (newln)))
            ((thunkElm? elm)
             (begin (and ugly? (display "#<thunk ( "))
                    (map (lviv-pp #f ugly?) (thunkElm->elm elm))
                    (and ugly? (display ")>"))
                    (newln)))
            ((lambda? elm)
             (begin (and ugly? (display "#<lambda ( "))
                    (map (lviv-pp #f ugly?) (lambda-code elm))
                    (and ugly? (display ") ( "))
                    (map (lviv-pp #f ugly?)
                         ((if (lambda-reverse? elm) reverse values)
                          (lambda-args elm)))
                    (and ugly? (begin (display ")")
                                        (display ">")))
                    (newln)))
            ((primitive? elm)
             (begin (and ugly? (display "#<primitive "))
                    (if (primitive-reverse? elm) (display ":"))
                    (display (primitive-id elm))
                    (and ugly? (display ">"))
                    (newln)))
            ((and (list? elm) (pair? elm))
             (begin (display "( ")
                    (map (lviv-pp #f ugly?) elm)
                    (display " )")
                    (newln)))
            ((pair? elm)
             (begin (display "( ")
                    ((lviv-pp #f ugly?) (car elm))
                    (display ". ")
                    ((lviv-pp #f ugly?) (cdr elm))
                    (display " )")
                    (newln)))
            ((and (number? elm)
                  (or (and (inexact? elm) (> (abs (xpon elm)) 3))
                      (> (abs (xpon elm)) 6)))
             (begin (display (eng elm))
                    (newln)))
            ((symbol? elm)
                  (begin (display "*")
                         (display elm)
                         (newln)))
            ((string? elm)
                  (begin (display "\"")
                         (display elm)
                         (display "\"")
                         (newln)))
            (else
              (begin (display elm)
                     (newln)))))))

; pretty-print the stack
(define (lviv-ppstack stack eDepth pretty?)
  (let ((depth (if (eRight? eDepth)
                 (fromLeftRight eDepth)
                 _stack_display_depth))
        (pprint (if (eRight? pretty?)
                  (not (fromLeftRight pretty?))
                  #t)))
    (map (lviv-pp #t pprint)
         (reverse (take (number->int depth) stack)))))

; pretty-print the environment, surrounding
; each level of the env with a { }
(define (lviv-ppenv env)
  (if (null? env)
    (newline)
    (begin (display "{\n")
           (map (lviv-pp #t) (car env))
           (display "}\n")
           (lviv-ppenv (cdr env)))))

