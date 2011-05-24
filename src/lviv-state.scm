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
; **** STATE ****
; ***************
; functions relating to interpreter state

; an empty stack
(define mkEmptyStack (lambda () '()))

; an empty environment
(define mkEmptyEnv (lambda () (cons '() '())))

; an interpreter state is just (stack . env)
(define mkEmptyState (lambda () (cons (mkEmptyStack) (mkEmptyEnv))))

; retrieve stack from the state
(define stGetStack car)

; retrieve env from the state
(define stGetEnv cdr)

; update environment
(define (stUpdateEnv oldState newEnv) (set-cdr! oldState newEnv))
(define (stUpdateStack oldState newStack) (set-car! oldState newStack))

