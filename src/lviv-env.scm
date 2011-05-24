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

; #############
; #### ENV #### 
; #############
; environment updates, et cetera

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

(define (envAddMany env items)
  (foldl envAddBinding env items))

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

; lookup a binding, ascending the environment tree
(define (envLookupBinding env name)
  (if (null? env)
    (eLeft "not found") ; hit the top of the environment stack
    (letrec ((lookupHlp
               (lambda (bindings)
                 (cond ((null? bindings) (envLookupBinding (envParent env) name))
                       ((eq? name (car (firstBinding bindings)))
                        (eRight (cdr (firstBinding bindings))))
                       (else (lookupHlp (restBindings bindings)))))))
      (lookupHlp (envBindings env)))))
(define stEnvLookupBinding (stEnvBindOp envLookupBinding))

; the "define" stackop
; pops a name and value off the stack.
; If the name is a static symbol, it
; updates the attached environment. Otherwise,
; it updates the current environment.
(define (stDefine state)
  (let* ((fnIdE (stStackPop state))
         (fnId (fromLeftRight fnIdE))
         (fnVal (delay (stStackPop state)))
         (saEnvItem
           (delay (cons (static/auto-symbol-sym fnId)
                        (fromLeftRight (force fnVal)))))
         (envItem
           (delay (cons fnId
                        (fromLeftRight (force fnVal))))))
    (cond ((eLeft? fnIdE) fnIdE)
          ((not (symbol-elm? fnId))
           (rewind state (list fnId) "invalid identifier"))
          ((eLeft? (force fnVal))
           (rewind state (list fnId) (fromLeftRight (force fnVal))))
          ((static-symbol-elm? fnId)
           (eRight
             (envUpdateBinding (static-symbol-env fnId)
                               (force saEnvItem))))
          ((auto-symbol-elm? fnId)
           (eRight (stEnvUpdateBinding state (force saEnvItem))))
          (else
           (eRight (stEnvUpdateBinding state (force envItem)))))))

; as above, but creates a lambda and
; puts it into the environment.
; a static symbol's environment is respected
; shouldn't lambda just put the result back on
; the stack and rely on define to put it into
; the environment? otherwise, we're replicating
; code unnecessarily
(define (stLambda state)
  (let* ((fnLArgs (stStackNPop state 3))
         (fnId (delay (caddr (fromLeftRight fnLArgs))))
         (fnArgs (delay (cadr (fromLeftRight fnLArgs))))
         (fnxCode (delay (car (fromLeftRight fnLArgs))))
         (fnCode 
           (delay
             (if (list? (force fnxCode))
               (force fnxCode)
               (list (force fnxCode)))))
         (fnLambda
           (lambda (env) (mkLambda (force fnCode) (force fnArgs) env))))
    (cond ((eLeft? fnLArgs) fnLArgs)
          ((not (symbol-elm? (force fnId)))
           (rewind state (reverse (fromLeftRight fnLArgs)) "invalid identifier"))
          ((static-symbol-elm? (force fnId))
           (eRight
             (envUpdateBinding (static-symbol-env (force fnId))
                               (cons (static/auto-symbol-sym (force fnId))
                                     (fnLambda (static-symbol-env (force fnId)))))))
          ((auto-symbol-elm? (force fnId))
           (eRight
             (stEnvUpdateBinding state (cons (static/auto-symbol-sym (force fnId))
                                             (fnLambda (stGetEnv state))))))
          (else
            (eRight
              (stEnvUpdateBinding state (cons (force fnId) (fnLambda (stGetEnv state)))))))))

; as above, but creates a primitive binding.
; Seems like this should also create a primitive binding
; that then gets put into the environment using the
; `define` construct rather than modifying the env
; directly
(define (stPrimitive state)
  (let* ((fnLArgs (stStackNPop state 2))
         (fnId (delay (cadr (fromLeftRight fnLArgs))))
         (fnArity (delay (car (fromLeftRight fnLArgs))))
         (fnBinding (lambda (id) (mkPrimBinding id (force fnArity))))
         (saSym (delay (static/auto-symbol-sym (force fnId)))))
    (cond ((eLeft? fnLArgs) fnLArgs)
          ((not (symbol-elm? (force fnId)))
           (rewind state (reverse (fromLeftRight fnLArgs)) "invalid identifier"))
          ((static-symbol-elm? (force fnId))
           (eRight
             (envUpdateBinding (static-symbol-env (force fnId))
                               (cons (force saSym)
                                     (fnBinding (force saSym))))))
          ((auto-symbol-elm? (force fnId))
           (eRight
             (stEnvUpdateBinding state (cons (force saSym)
                                             (fnBinding (force saSym))))))
          (else
            (eRight
              (stEnvUpdateBinding state (cons (force fnId)
                                              (fnBinding (force fnId)))))))))


