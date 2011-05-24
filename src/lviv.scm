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

(include "lviv-misc.scm")
(include "lviv-state.scm")
(include "lviv-stack.scm")
(include "lviv-env.scm")
(include "lviv-exceptions.scm")
(include "lviv-symbols.scm")
(include "lviv-funcalls.scm")
(include "lviv-repl.scm")
(include "lviv-prelude.scm")

; ###############
; #### TESTS ####
; ###############

(define (test x msg)
  (or x (raise msg)))

(define (testLookup x val)
    (test (and (eRight? (stEnvLookupBinding myState x))
               (equal? (fromLeftRight (stEnvLookupBinding myState x)) val))
          (string-append (symbol->string x) " lookup failed")))

(define (testStack val msg)
  (test (equal? (stGetStack myState) val) msg))

(stStackPush myState 1)
(stStackPush myState 2)
(stStackPush myState 3)
(stStackPush myState 4)
(stStackPush myState 5)
(stStackPush myState 2)
(testStack '(2 5 4 3 2 1) "push tests failed")

(stStackSwap myState)
(testStack '(5 2 4 3 2 1) "swap test failed")

(stStackDrop myState)
(testStack '(2 4 3 2 1) "drop test failed")

(stStackDropN myState)
(testStack '(2 1) "dropN test failed")

(stStackClear myState)
(testStack '() "clear test failed")

(stStackPush myState 1)
(stStackPush myState 2)
(stStackPush myState 3)
(stStackPush myState 4)
(stStackPush myState 5)
(stStackRollN myState)
(testStack '(5 4 3 2 1) "opN not enough args test failed")
(stStackPush myState 3)
(stStackRollN myState)
(testStack '(3 5 4 2 1) "rollN test failed")

(stStackPush myState 2)
(stStackUnrollN myState)
(testStack '(5 3 4 2 1) "unrollN test failed")

(stStackDup myState)
(testStack '(5 5 3 4 2 1) "dup test failed")

(stStackPush myState 3)
(stStackDupN myState)
(testStack '(5 5 3 5 5 3 4 2 1) "dupN test failed")

(stStackPush myState 'a)
(stStackDupN myState)
(testStack '(a 5 5 3 5 5 3 4 2 1) "opN non-numeric test failed")

(stStackOver myState)
(testStack '(5 a 5 5 3 5 5 3 4 2 1) "over test failed")

(stStackPush myState '5)
(stStackPickN myState)
(testStack '(3 5 a 5 5 3 5 5 3 4 2 1) "pickN test failed")

(stPrimCall myState (mkPrimBinding '+ 2))
(testStack '(8 a 5 5 3 5 5 3 4 2 1) "hidden primitive test failed")

(stStackDropN myState)
(testStack '(2 1) "dropN test failed")

(stStackPush myState '#t)
(stStackDropUnless myState)
(testStack '(2 1) "#t dropUnless test failed")

(stStackPush myState '#f)
(stStackDropUnless myState)
(testStack '(1) "#f dropUnless test failed")

(stStackPush myState 5)
(stStackDropUnless myState)
(testStack '(5 1) "non-bool dropUnless test failed")

(stStackPush myState #f)
(stStackDropIf myState)
(testStack '(5 1) "#f dropIf test failed")

(stStackPush myState #t)
(stStackDropIf myState)
(testStack '(1) "#t dropIf test failed")

(stStackDropIf myState)
(testStack '(1) "non-bool dropIf test failed")

(stStackPush myState 5)
(stStackPush myState #t)
(stStackSwapIf myState)
(testStack '(1 5) "#t swapIf test failed")

(stStackPush myState 5)
(stStackPush myState #f)
(stStackSwapIf myState)
(testStack '(5 1 5) "#f swapIf test failed")

(stStackSwapIf myState)
(testStack '(5 1 5) "non-bool swapIf test failed")

(stStackPush myState #f)
(stStackPush myState #t)
(stStackSwapUnless myState)
(testStack '(#f 5 1 5) "#t swapUnless test failed")

(stStackSwapUnless myState)
(testStack '(1 5 5) "#f swapUnless test failed")

(stStackSwapUnless myState)
(testStack '(1 5 5) "non-bool swapUnless test failed")

(stEnvUpdateBinding myState '(a . 1))
(stEnvUpdateBinding myState '(b . 2))
(stEnvUpdateBinding myState '(c . 3))
(stEnvUpdateBinding myState '(d . 4))
(stEnvUpdateBinding myState '(e . 5))
(stEnvUpdateBinding myState '(a . 6))
(stEnvDelBinding myState 'd)

(test (eLeft? (stEnvLookupBinding myState 'd)) "d still bound!?")
(testLookup 'e 5)

(stEnvNewChild myState)
(stEnvUpdateBinding myState '(f 100))
(stEnvUpdateBinding myState '(b 200))

(testLookup 'e 5)
(testLookup 'f '(100))
(testLookup 'b '(200))
(test (eLeft? (stEnvLookupBinding myState 'd)) "d still bound!? (2)")

(display (stEnvLookupBinding myState 'c)) (newline)
(display (stEnvLookupBinding myState 'd)) (newline)
(display (stEnvLookupBinding myState 'b)) (newline)

;(display myState) (newline)
(stEnvParent myState)
(stEnvParent myState)
(stEnvParent myState)
(stEnvParent myState)
;(display myState) (newline)

(test (stGlobalEnv? myState) "should be global env here")

(stEnvNewChild myState)
(stEnvUpdateBinding myState '(f 100))
(stEnvUpdateBinding myState '(b 200))

(test (not (stGlobalEnv? myState)) "should not be global env here")

(stEnvParent myState)
(stEnvParent myState)
(test (stGlobalEnv? myState) "should be global env here")

(lviv-apply myState (lviv-eval myState '+))

(test (equal? (stGetStack myState) '(6 5)) "state of stack is wrong after +")

(lviv-apply myState (lviv-eval myState '@b))
(lviv-apply myState (lviv-eval myState '@a))
;(display myState) (newline)

(test (eLeft? 
        (with-exception-catcher (exceptionHandler #f)
         (lambda () (stPrimCall myState (lviv-eval myState '+)))))
        "call to + failed to fail")
(test (equal? (stGetStack myState) (list (mkAutoSymbolElm 'a) (mkAutoSymbolElm 'b) 6 5))
      "stack is in wrong state after type failure")
(testLookup 'cdr (mkPrimBinding 'cdr 1))

;(display myState) (newline)

(lviv-repl myState #f)

