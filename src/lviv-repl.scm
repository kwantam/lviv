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
    (cond ((eq? item 'nop) item) ; nop is same in the AST
          ((eq? item 'env) item) ; env is same in the AST
          ((eq? item 'if) item)
          ((static-symbol? item) ; &foo -> (& foo env)
           (mkStaticSymbolElm (static-symbol->symbol item)
                              (stGetEnv state)))
          ((auto-symbol? item)   ; @foo -> (@ foo)
           (mkAutoSymbolElm (auto-symbol->symbol item)))
          ((posn-symbol? item)   ; !1   -> (! 1)
           (mkPosnRefElm (posn-symbol->symbol item)))
          ((quote-symbol? item)  ; *bar -> bar
           (quote-symbol->symbol item))
          ((stStackOp? item) (mkStackOpElm (stStackOp? item))) ; primitive stack op
          ((static-symbol-elm? item) (lookupElm (caddr item) (cadr item)))
          ((auto-symbol-elm? item) (lookupElm (stGetEnv state) (cadr item)))
          ((posn-symbol-elm? item) (eLeft "not implemented"))
          ((reverse-symbol? item) ; :cons -> cons in reverse
           (let* ((iLBind (stEnvLookupBinding state (reverse-symbol->symbol item)))
                  (iBind (fromLeftRight iLBind)))
             (cond ((eLeft? iLBind) iLBind)
                   ((primitive? iBind) (prim-reverse iBind))
                   ((lambda? iBind) (lambda-reverse iBind))
                   (else (eLeft "cannot reverse non-op")))))
          ((symbol? item)        ; look up symbol in present env
           (let ((iLBind (stEnvLookupBinding state item)))
             (if (eLeft? iLBind) ; did lookup succeed?
               (mkAutoSymbolElm item) ; no---make auto symbol
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
  (cond ((eq? item 'nop) (eRight '())) ; nop does nothing         **TODO make this a stackOp
        ((eq? item 'env) ; env will show the present environment  **TODO make this a stackOp
         (begin (pp (stGetEnv state))
                (newline)
                (eRight '())))
        ((eLeft? item) item)
        ((eq? item 'if) ; this is a really slow way of making an if, but it's OK for now
         ; <consequent> <alternative> <boolean> if				  **TODO make this a stackOp
         ; this could just be a 3-arg lambda:
         ; (consequent alternative bool swapUnless drop thunk apply) (bool,alternative,consequent) if lambda
         (begin (stStackSwapUnless state)
                (stStackDrop state)
                (stStackThunk state)
                (stApply state)))
        ((symbol-elm? item)
         (stStackPush state item))
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

; pretty-print the stack. TODO: make this prettier
(define (lviv-ppstack stack eDepth)
  (let ((depth (if (and (eRight? eDepth) (exact? (fromLeftRight eDepth)))
                 (fromLeftRight eDepth)
                 _stack_display_depth)))
    (map pp (reverse (take depth stack)))))


