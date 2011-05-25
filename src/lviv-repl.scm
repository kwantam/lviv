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
           ((applyMap state)                    ; apply as if just typed in
            (thunkElm->elm fnArg)))
          (else (lviv-apply state fnArg)))))    ; otherwise, just apply it like anything else

; the main eval procedure
(define (lviv-eval state item)
  (let ((lookupElm (lambda (env name)
                     (let ((lkRef (envLookupBinding env name)))
                       (if (eLeft? lkRef)
                         (eLeft "lookup failed")
                         (fromLeftRight lkRef))))))
    (cond ((static-symbol? item) ; &foo -> (& foo env)
           (mkStaticSymbolElm (static-symbol->symbol item)
                              (stGetEnv state)))
; nyi     ;((posn-symbol? item)   ; !1   -> (! 1)
; nyi     ; (mkPosnRefElm (posn-symbol->symbol item)))
          ((quote-symbol? item)  ; *bar -> bar
           (quote-symbol->symbol item))
          ((reverse-symbol? item) ; :cons -> cons in reverse
           (let* ((iLBind (stEnvLookupBinding state (reverse-symbol->symbol item)))
                  (iBind (fromLeftRight iLBind)))
             (cond ((eLeft? iLBind) iLBind)
                   ((primitive? iBind) (prim-reverse iBind))
                   ((lambda? iBind) (lambda-reverse iBind))
                   (else (eLeft "can only reverse lambda or primitive")))))
          ((stStackOp? item) (mkStackOpElm (stStackOp? item) item)) ; stackOp
          ((static-symbol-elm? item) (lookupElm (static-symbol-env item)
                                                (static-symbol-sym item)))
          ((posn-symbol-elm? item) (eLeft "not implemented"))
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

; we use read-line rather than read so that we get all of the inputs at once,
; and don't end up printing the state of the stack between each element in
; the input as we apply them
(define (lviv-repl state input) ; repl
  (if (eq? input '#!eof)
    #f
    (begin 
      (if (string? input)
        (with-exception-catcher (exceptionHandler #t) (lambda ()
        ((applyMap state)
         (call-with-input-string input read-all)))))
      (lviv-ppstack (stGetStack state) (stEnvLookupBinding state '_stack_display_depth))
      (display "> ")
      (lviv-repl state (read-line)))))

; default stack display depth
(define _stack_display_depth 10)

; pretty-print the stack
(define (lviv-ppstack stack eDepth)
  (letrec ((depth (if (and (eRight? eDepth) (exact? (fromLeftRight eDepth)))
                    (fromLeftRight eDepth)
                    _stack_display_depth))
           (lpp 
             (lambda (newline?)
               (lambda (elm)
                 (let ((newln (lambda () (if newline? (display "\n") (display " ")))))
                   (cond ((static-symbol-elm? elm)
                          (begin (display "&")
                                 (display (static-symbol-sym elm))
                                 (display " -> (env ")
                                 (display (static-symbol-sn elm))
                                 (display ")")
                                 (newln)))
                         ((posn-symbol-elm? elm)
                          (begin (display "!")
                                 (display (posn-symbol-elm elm))
                                 (newln)))
                         ((stackOpElm? elm)
                          (begin (display "#<stackOp ")
                                 (display (stackOpElm-sym elm))
                                 (display ">")
                                 (newln)))
                         ((thunkElm? elm)
                          (begin (display "#<thunk ( ")
                                 (map (lpp #f) (thunkElm->elm elm))
                                 (display ")>")
                                 (newln)))
                         ((lambda? elm)
                          (begin (display "#<lambda ( ")
                                 (map (lpp #f) (lambda-code elm))
                                 (display ") ( ")
                                 (map (lpp #f) (lambda-args elm))
                                 (display ")")
                                 (display ">")
                                 (newln)))
                         ((primitive? elm)
                          (begin (display "#<primitive ")
                                 (display (primitive-id elm))
                                 (display ">")
                                 (newln)))
                         (else
                           (begin (display elm)
                                  (newln)))))))))
    (map (lpp #t) (reverse (take depth stack)))))

