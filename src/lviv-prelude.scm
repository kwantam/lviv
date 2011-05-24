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

; #################
; #### PRELUDE ####
; #################
; eventually, most of this will be defined in lviv directly
; rather than via lviv wrapped in scheme calls (but of course
; file reading will have to be implemented first!)

(define myState (mkEmptyState))

((applyMap myState) '(3.141592653589793238462643 *pi define))

((applyMap myState) '(2 + primitive))
((applyMap myState) '(2 - primitive))
((applyMap myState) '(2 / primitive))
((applyMap myState) '(2 * primitive))
((applyMap myState) '(2 cons primitive))
((applyMap myState) '(2 eq? primitive))
((applyMap myState) '(2 expt primitive))
((applyMap myState) '(1 sqrt primitive))

(define (add-cxrs state n)
  (letrec ((nums (take n '(1 2 4 8 16)))
           (bitAD
             (lambda (cnt)
               (apply string-append
                 (map (lambda (x)
                        (if (= (modulo (quotient cnt x) 2) 0) "a" "d"))
                      nums))))
           (acHlp
             (lambda (cnt) 
               (if (= (expt 2 n) cnt) #t
                 (let ((nxName (string->symbol (string-append "c" (bitAD cnt) "r"))))
                   (stEnvUpdateBinding state
                                       (cons nxName
                                             (mkPrimBinding nxName 1)))
                   (acHlp (+ cnt 1)))))))
    (acHlp 0)))

;(add-cxrs myState 5)
;only defined up to 4 levels, not 5 as I'd previously believed
(add-cxrs myState 4)
(add-cxrs myState 3)
(add-cxrs myState 2)
(add-cxrs myState 1)

(stEnvUpdateBinding myState (cons 'nil '()))


