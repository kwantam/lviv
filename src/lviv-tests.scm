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
; #### TESTS ####
; ###############

(define (test x msg)
  (or x (raise msg)))

(define (testLookup x val)
    (test (and (eRight? (stEnvLookupBinding lvivState x))
               (equal? (fromLeftRight (stEnvLookupBinding lvivState x)) val))
          (string-append (symbol->string x) " lookup failed")))

(define (testStack val msg)
  (test (equal? (stGetStack lvivState) val) msg))

(stStackPush lvivState 1)
(stStackPush lvivState 2)
(stStackPush lvivState 3)
(stStackPush lvivState 4)
(stStackPush lvivState 5)
(stStackPush lvivState 2)
(testStack '(2 5 4 3 2 1) "push tests failed")

(stStackSwap lvivState)
(testStack '(5 2 4 3 2 1) "swap test failed")

(stStackDrop lvivState)
(testStack '(2 4 3 2 1) "drop test failed")

(stStackDropN lvivState)
(testStack '(2 1) "dropN test failed")

(stStackClear lvivState)
(testStack '() "clear test failed")

(stStackPush lvivState 1)
(stStackPush lvivState 2)
(stStackPush lvivState 3)
(stStackPush lvivState 4)
(stStackPush lvivState 5)
(stStackRollN lvivState)
(testStack '(5 4 3 2 1) "opN not enough args test failed")
(stStackPush lvivState 3)
(stStackRollN lvivState)
(testStack '(3 5 4 2 1) "rollN test failed")

(stStackPush lvivState 2)
(stStackUnrollN lvivState)
(testStack '(5 3 4 2 1) "unrollN test failed")

(stStackDup lvivState)
(testStack '(5 5 3 4 2 1) "dup test failed")

(stStackPush lvivState 3)
(stStackDupN lvivState)
(testStack '(5 5 3 5 5 3 4 2 1) "dupN test failed")

(stStackPush lvivState 'a)
(stStackDupN lvivState)
(testStack '(a 5 5 3 5 5 3 4 2 1) "opN non-numeric test failed")

(stStackOver lvivState)
(testStack '(5 a 5 5 3 5 5 3 4 2 1) "over test failed")

(stStackPush lvivState '5)
(stStackPickN lvivState)
(testStack '(3 5 a 5 5 3 5 5 3 4 2 1) "pickN test failed")

(stPrimCall lvivState (mkPrimBinding '+ 2))
(testStack '(8 a 5 5 3 5 5 3 4 2 1) "hidden primitive test failed")

(stStackDropN lvivState)
(testStack '(2 1) "dropN test failed")

(stStackPush lvivState '#t)
(stStackDropUnless lvivState)
(testStack '(2 1) "#t dropUnless test failed")

(stStackPush lvivState '#f)
(stStackDropUnless lvivState)
(testStack '(1) "#f dropUnless test failed")

(stStackPush lvivState 5)
(stStackDropUnless lvivState)
(testStack '(5 1) "non-bool dropUnless test failed")

(stStackPush lvivState #f)
(stStackDropIf lvivState)
(testStack '(5 1) "#f dropIf test failed")

(stStackPush lvivState #t)
(stStackDropIf lvivState)
(testStack '(1) "#t dropIf test failed")

(stStackDropIf lvivState)
(testStack '(1) "non-bool dropIf test failed")

(stStackPush lvivState 5)
(stStackPush lvivState #t)
(stStackSwapIf lvivState)
(testStack '(1 5) "#t swapIf test failed")

(stStackPush lvivState 5)
(stStackPush lvivState #f)
(stStackSwapIf lvivState)
(testStack '(5 1 5) "#f swapIf test failed")

(stStackSwapIf lvivState)
(testStack '(5 1 5) "non-bool swapIf test failed")

(stStackPush lvivState #f)
(stStackPush lvivState #t)
(stStackSwapUnless lvivState)
(testStack '(#f 5 1 5) "#t swapUnless test failed")

(stStackSwapUnless lvivState)
(testStack '(1 5 5) "#f swapUnless test failed")

(stStackSwapUnless lvivState)
(testStack '(1 5 5) "non-bool swapUnless test failed")

(stEnvUpdateBinding lvivState (cons 'a 1))
(stEnvUpdateBinding lvivState (cons 'b 2))
(stEnvUpdateBinding lvivState (cons 'c 3))
(stEnvUpdateBinding lvivState (cons 'd 4))
(stEnvUpdateBinding lvivState (cons 'e 5))
(stEnvUpdateBinding lvivState (cons 'a 6))
((stEnvDelBinding #f) lvivState 'd)

(test (eLeft? (stEnvLookupBinding lvivState 'd)) "d still bound!?")
(testLookup 'e 5)

(stEnvNewChild lvivState)
(stEnvUpdateBinding lvivState '(f 100))
(stEnvUpdateBinding lvivState '(b 200))

(testLookup 'e 5)
(testLookup 'f '(100))
(testLookup 'b '(200))

(test (eLeft? (stEnvLookupBinding lvivState 'd)) "d still bound!? (2)")

(stEnvParent lvivState)
(stEnvParent lvivState)
(stEnvParent lvivState)
(stEnvParent lvivState)

(test (stGlobalEnv? lvivState) "should be global env here")

(stEnvNewChild lvivState)
(stEnvUpdateBinding lvivState '(f 100))
(stEnvUpdateBinding lvivState '(b 200))

(test (not (stGlobalEnv? lvivState)) "should not be global env here")

(stEnvParent lvivState)
(stEnvParent lvivState)
(test (stGlobalEnv? lvivState) "should be global env here")

(lviv-apply lvivState (lviv-eval lvivState '+))

(testStack '(6 5) "state of stack is wrong after +")

(lviv-apply lvivState (lviv-eval lvivState '*b))
(lviv-apply lvivState (lviv-eval lvivState '*a))

(test (eLeft? 
        (with-exception-catcher (exceptionHandler #f)
         (lambda () (stPrimCall lvivState (lviv-eval lvivState '+)))))
        "call to + failed to fail")

(testStack '(a b 6 5) "stack is in wrong state after type failure")

(testLookup 'cdr (mkPrimBinding 'cdr 1))

((applyMap lvivState) '(() cons cons (-) append (*a *b) lambda apply))
(testStack '(1) "stack is in wrong state after lambda")

((applyMap lvivState) '((&a +) cons thunk apply))
(testStack '(7) "stack is in wrong state after thunk")

((applyMap lvivState) '(*a undef))
(test (eLeft? (stEnvLookupBinding lvivState 'a)) "a still bound after undef?")

((applyMap lvivState) '(*a define))
((applyMap lvivState) '((&b &a *a * +) ((*a . (&a 2 +))) let))
(testStack '(65) "stack is in wrong state after let")

((applyMap lvivState) '(drop))

((applyMap lvivState) '(*a *b *c *e undef undef undef undef))
