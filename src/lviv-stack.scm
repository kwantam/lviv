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
; **** STACK ****
; ***************
; operations that work directly on the stack, generally
; constituting or used by the lviv primitive stack operations

; update stack, returning a value
; supplied function should return a value such that
; result is (car (f stack))
; newStk is (cdr (f stack))
(define (stStackUpd2 f state)
  (let* ((fValue (f (stGetStack state)))
         (fResult (car fValue))
         (fRemain (cdr fValue)))
    (stUpdateStack state fRemain)
    fResult))

; update stack, returning new stack
; cannot return an error
(define (stStackUpd f state)
  (let ((fValue (f (stGetStack state))))
    (stUpdateStack state fValue)
    (eRight '())))

; push onto stack
(define (stackPush var)
  (lambda (stack) (cons var stack)))
(define (stStackPush state var) 
  (stStackUpd (stackPush var) state))

; push a list of values back onto the stack
(define (stackNPush var)
  (lambda (stack) (append var stack)))
(define (stStackNPush state var)
  (stStackUpd (stackNPush var) state))

; stack depth
(define depth length)
(define (stStackDepth state) (stStackPush state (depth (stGetStack state))))

; pop off stack
; when popping, return (cons <popped value> <new stack>)
; since we represent the stack as a list, pop is just
; the identity function
(define (stackPop stack)
  (if (> (depth stack) 0)
    (cons (eRight (car stack)) (cdr stack))
    (cons (eLeft "pop: stack empty") '())))

(define (stStackPop state) (stStackUpd2 stackPop state))

; pop n items off stack
; this is used when executing functions
(define (stackPopN n)
  (lambda (stack)
    (if (< (depth stack) n)
      (cons (eLeft "popN: not enough arguments") stack)
      (letrec ((sPopNHlp
                 (lambda (stack accum n)
                   (if (= n 0)
                     (cons (eRight accum) stack)
                     (sPopNHlp (cdr stack) (cons (car stack) accum) (- n 1))))))
        (sPopNHlp stack '() n)))))
(define (stStackNPop state n) (stStackUpd2 (stackPopN n) state))

; swap the 0th and 1st elements of the stack
(define (stackSwap stack)
  (if (> (depth stack) 1)
    (cons (eRight '()) (cons (cadr stack) (cons (car stack) (cddr stack))))
    (cons (eLeft "swap: not enough elements") stack)))
(define (stStackSwap state) (stStackUpd2 stackSwap state))

; drop the 0th element
(define (stackDrop stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cdr stack))
    (cons (eLeft "drop: stack empty") '())))
(define (stStackDrop state) (stStackUpd2 stackDrop state))

; clear the stack, i.e., replace it with emptyState
(define stackClear (lambda (x) (mkEmptyStack)))
(define (stStackClear state) (stStackUpd stackClear state))

; a generalized stack operation that takes the 0th elem
; off the stack, expecting an integer, and produces a
; modified stack as a result
(define (stackOpN f)
  (lambda (stack)
    (let* ((popVal (stackPop stack))
           (popEither (car popVal))
           (popRes (fromLeftRight popEither))
           (popRem (cdr popVal)))
      (cond ((eLeft? popEither) popVal)
            ((number? popRes)
             (if (<= (number->int popRes) (depth popRem))
               (cons (eRight '()) (f (number->int popRes) popRem))
               (cons (eLeft "stackOpN: not enough elements") stack)))
            (else (cons (eLeft "stackOpN: non-numeric argument") stack))))))

; drop N elements after the 0th
(define stackDropN
  (stackOpN (lambda (n st) (iterateNOrNull cdr n st))))
(define (stStackDropN state) (stStackUpd2 stackDropN state))

; roll the top N elements after the 0th
(define stackRollN (stackOpN rollN))
(define (stStackRollN state) (stStackUpd2 stackRollN state))

; unroll the top N elements after the 0th
(define stackUnrollN (stackOpN unrollN))
(define (stStackUnrollN state) (stStackUpd2 stackUnrollN state))

; dup the top element
(define (stackDup stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cons (car stack) stack))
    (cons (eLeft "dup: stack empty") '())))
(define (stStackDup state) (stStackUpd2 stackDup state))

; dup the first N elements after the 0th
(define stackDupN (stackOpN dupN))
(define (stStackDupN state) (stStackUpd2 stackDupN state))

; duplicate the second element on the stack
(define (stackOver stack)
  (if (> (depth stack) 1)
    (cons (eRight '()) (cons (cadr stack) stack))
    (cons (eLeft "over: not enough elements") stack)))
(define (stStackOver state) (stStackUpd2 stackOver state))

; duplicate the nth element on the stack
(define stackPickN (stackOpN pickN))
(define (stStackPickN state) (stStackUpd2 stackPickN state))

; generalized stack operation that takes the 0th elem
; off the stack and evaluates it for truth. If true,
; a supplied stackop is executed as long as there are
; sufficient elements in the stack
(define (stackOpBool bool f)
  (lambda (stack)
    (let* ((popVal (stackPop stack))
           (popEither (car popVal))
           (popRes (fromLeftRight popEither))
           (popRem (cdr popVal)))
      (with-exception-catcher
        (lambda (x) (cons (eLeft "type error") stack))
        (lambda ()
          (cond ((eLeft? popEither) popVal)
                ((bool popRes) (f popRem))
                (else (cons (eRight '()) popRem))))))))

; swapIf
(define stackSwapIf
  (stackOpBool =true? stackSwap))
(define (stStackSwapIf state) (stStackUpd2 stackSwapIf state))

; swapUnless
(define stackSwapUnless
  (stackOpBool =false? stackSwap))
(define (stStackSwapUnless state) (stStackUpd2 stackSwapUnless state))

; dropIf
(define stackDropIf
  (stackOpBool =true? stackDrop))
(define (stStackDropIf state) (stStackUpd2 stackDropIf state))

; dropUnless
(define stackDropUnless
  (stackOpBool =false? stackDrop))
(define (stStackDropUnless state) (stStackUpd2 stackDropUnless state))

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

(define (stackOp? symb f)
  (lambda (op) (if (eq? symb op) f #f)))

; stackop lookup
(define (stStackOp? op)
  (or ((stackOp? 'depth stStackDepth) op)
      ((stackOp? 'swap stStackSwap) op)
      ((stackOp? 'drop stStackDrop) op)
      ((stackOp? 'clear stStackClear) op)
      ((stackOp? 'dropN stStackDropN) op)
      ((stackOp? 'roll stStackRollN) op)
      ((stackOp? 'unroll stStackUnrollN) op)
      ((stackOp? 'dup stStackDup) op)
      ((stackOp? 'dupN stStackDupN) op)
      ((stackOp? 'over stStackOver) op)
      ((stackOp? 'pick stStackPickN) op)
      ((stackOp? 'swapIf stStackSwapIf) op)
      ((stackOp? 'swapUnless stStackSwapUnless) op)
      ((stackOp? 'dropIf stStackDropIf) op)
      ((stackOp? 'dropUnless stStackDropUnless) op)
      ((stackOp? 'uncons stStackUncons) op)
      ((stackOp? 'define stDefine) op)
      ((stackOp? 'eval stEval) op)
      ((stackOp? 'apply stApply) op)
      ((stackOp? 'thunk stStackThunk) op)
      ((stackOp? 'lambda stLambda) op)
      ((stackOp? 'primitive stPrimitive) op)
      ))


