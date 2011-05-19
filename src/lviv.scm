
;#lang r5rs
; Except that somehow DrRacket's implementation is case sensitive and stuff

; nil is just the empty list
(define nil '())

; an empty stack
(define emptyStack '())

; an empty environment
(define emptyEnv (cons '() '()))

; an interpreter state is just (stack . env)
(define emptyState (cons emptyStack emptyEnv))

; retrieve stack from the state
(define stGetStack car)

; retrieve env from the state
(define stGetEnv cdr)

; retrieve bindings from the env
(define stGetBindings cadr)
(define envGetBindings car)

; retrieve enclsure from the env
(define stGetEnclosure cddr)
(define envGetEnclosure cdr)

; is this the global environment?
(define (global? env) (eq? (envGetEnclosure env) nil))

; update environment
(define (stUpdateEnv oldState newEnv) (set-cdr! oldState newEnv))
; update stack
(define (stUpdateStack oldState newStack) (set-car! oldState newStack))

; push onto stack
(define (stackPush stack var) (cons var stack))
(define (stStackPush state var) 
  (begin (stUpdateStack state (stackPush (stGetStack state) var))
         state))

; pop off stack
; when popping, return (cons <popped value> <new stack>)
; since we represent the stack as a list, pop is just
; the identity function
(define stackPop values)
(define stackPopResult car)
(define stackPopRemain cdr)
; pop off a value, and update the stack in the state
(define (stStackPop state) (let* ((popValue (stackPop (stGetStack state)))
                                  (popResult (stackPopResult popValue))
                                  (popRemain (stackPopRemain popValue)))
                             (stUpdateStack state popRemain)
                             popResult))


