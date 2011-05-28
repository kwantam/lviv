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
(define (applyMap state)
  (lambda (elm)
  (map (lambda (x) 
         (lviv-apply state (lviv-eval state x)))
       elm)))

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
             (begin ((applyMap state) (car thunkCodeParts)) ; call first part of thunk
                    (lviv-apply                             ; then tail call last part
                      state
                      (lviv-eval state
                                 (cadr thunkCodeParts)))))))
          (else (lviv-apply state fnArg)))))    ; otherwise, just apply it like anything else

; the main eval procedure
(define (lviv-eval state item)
  (let ((lookupElm (lambda (env name)
                     (let ((lkRef (envLookupBinding env name)))
                       (if (eLeft? lkRef)
                         (eLeft "lookup failed")
                         (fromLeftRight lkRef))))))
    (cond ((static-symbol? item) ; &foo -> (& foo env)
           (mkStaticSymbolElm item (stGetEnv state)))
          ((quote-symbol? item)  ; *bar -> bar
           (mkQuoteSymbolElm item))
          ((reverse-symbol? item) ; :cons -> cons in reverse
           (let* ((iLBind (stEnvLookupBinding state (reverse-symbol->symbol item)))
                  (iBind (fromLeftRight iLBind)))
             (cond ((eLeft? iLBind) iLBind)
                   ((primitive? iBind) (prim-reverse iBind))
                   ((lambda? iBind) (lambda-reverse iBind))
                   (else (eLeft "can only reverse lambda or primitive")))))
          ((static-symbol-elm? item) (lookupElm (static-symbol-env item)
                                                (static-symbol-sym item)))
          ((stStackOp? item) (mkStackOpElm (stStackOp? item) item)) ; stackOp
          ((symbol? item)        ; look up symbol in present env
           (let ((iLBind (stEnvLookupBinding state item)))
             (if (eLeft? iLBind) ; did lookup succeed?
               item ; no---make auto symbol
               (fromLeftRight iLBind)))) ; yes---pass it on
          ((lviv-tagged? item) item) ; all other lviv-tagged items are idempotent
          ((and (list? item) (pair? item)) ; has to be list and not nil
           (map (lambda (x) (lviv-eval state x)) item))
          ((pair? item) ; a pair gets car and cdr evaluated
           (cons (lviv-eval state (car item))
                 (lviv-eval state (cdr item))))
          (else item)))) ; otherwise, I guess it's idempotent

; apply the output from eval to the stack
(define (lviv-apply state item)
  ((lambda (result) (if (eLeft? result) (stackError result) result))
   (cond ((eLeft? item) item)
         ((stackOpElm? item) ((stackOpElm->stackop item) state))
         ((primitive? item) (stPrimCall state item))
         ((lambda? item) (stLambdaCall state item))
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
  (cond ((eq? input '#!eof) #f)
        (else
          (if input
            (let ((allInput (cons input (read-list))))
              (with-exception-catcher (exceptionHandler #t)
                                      (lambda () ((applyMap state) allInput)))))
          (stPrintStack state)
          (display "> ")
          (lviv-repl state (read)))))

; read in a file as if entered at the repl
(define (lviv-file state file)
  (let* ((fInput (with-exception-catcher
                   (exceptionHandler #f)
                   (lambda () (open-input-file file))))
         (fRead (delay (read-all fInput))))
    (if (eLeft? fInput)
      fInput
      (begin (with-exception-catcher
               (exceptionHandler #t)
               (lambda () ((applyMap state) (force fRead))))
             (stPrintStack state)))))

; default stack display depth
(define _stack_display_depth 10)

; prettyprint for lviv elements
(define (lviv-pp newline?)
  (lambda (elm)
    (let ((newln (lambda () (if newline? (display "\n") (display " ")))))
      (cond ((static-symbol-elm? elm)
             (begin (display "&")
                    (display (static-symbol-sym elm))
                    (display "(env#")
                    (display (static-symbol-sn elm))
                    (display ")")
                    (newln)))
            ((stackOpElm? elm)
             (begin (display "#<stackOp ")
                    (display (stackOpElm-sym elm))
                    (display ">")
                    (newln)))
            ((thunkElm? elm)
             (begin (display "#<thunk ( ")
                    (map (lviv-pp #f) (thunkElm->elm elm))
                    (display ")>")
                    (newln)))
            ((lambda? elm)
             (begin (display "#<lambda ( ")
                    (map (lviv-pp #f) (lambda-code elm))
                    (display ") ( ")
                    (map (lviv-pp #f) 
                         ((if (lambda-reverse? elm) reverse values)
                          (lambda-args elm)))
                    (display ")")
                    (display ">")
                    (newln)))
            ((primitive? elm)
             (begin (display "#<primitive ")
                    (if (primitive-reverse? elm) (display ":"))
                    (display (primitive-id elm))
                    (display ">")
                    (newln)))
            ((and (list? elm) (pair? elm))
             (begin (display "( ")
                    (map (lviv-pp #f) elm)
                    (display " )")
                    (newln)))
            ((pair? elm)
             (begin (display "( ")
                    ((lviv-pp #f) (car elm))
                    (display ". ")
                    ((lviv-pp #f) (cdr elm))
                    (display " )")
                    (newln)))
            (else
              (begin (display elm)
                     (newln)))))))

; pretty-print the stack
(define (lviv-ppstack stack eDepth)
  (letrec ((depth (if (eRight? eDepth)
                    (fromLeftRight eDepth)
                    _stack_display_depth)))
    (map (lviv-pp #t)
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

