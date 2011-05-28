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

(define lvivState (mkEmptyState))

(define (lviv-define-prim x arity . name)
  (if (null? name)
    ((applyMap lvivState) (quasiquote (,arity ,x primitive ,x define)))
    (let ((cName (car name)))
      ((applyMap lvivState) (quasiquote (,arity ,x primitive ,cName define))))))
(define (lviv-define-val x val)
  ((applyMap lvivState) (quasiquote (,val ,x define))))

; constants
(lviv-define-val 'pi 3.141592653589793238462643) ; more than we can actually represent
(lviv-define-val 'nil '())

; arithmetic
(lviv-define-prim '+ 2)
(lviv-define-prim '- 2)
(lviv-define-prim '/ 2)
(lviv-define-prim '* 2)

(define (inv x) (/ 1 x)) (lviv-define-prim 'inv 1)
(lviv-define-prim 'modulo 2 'mod)

(define (chs x) (* -1 x))
(lviv-define-prim 'chs 1 'neg)
(lviv-define-prim 'chs 1)
(lviv-define-prim 'abs 1)

(lviv-define-prim 'ceiling 1 'ceil)
(lviv-define-prim 'floor 1)
(lviv-define-prim 'number->int 1 'int)

(define (frac x) (- x (number->int x)))
(lviv-define-prim 'frac 1)

(define (pct y x) (* (/ y 100) x))
(lviv-define-prim 'pct 2 '%)

(define (pctOf y x) (* 100 (/ x y)))
(lviv-define-prim 'pctOf 2 '%t)

(define (pctCh y x) (* 100 (/ (- x y) y)))
(lviv-define-prim 'pctCh 2 '%ch)

; exponential and logarithmic
(lviv-define-prim 'expt 2 '^)
(define (xroot y x) (expt y (/ 1 x))) (lviv-define-prim 'xroot 2)
(define (cis x) (exp (* +i x))) (lviv-define-prim 'cis 1)
(define (sq x) (* x x)) (lviv-define-prim 'sq 1)
(lviv-define-prim 'sqrt 1)
(lviv-define-prim 'exp 1)
(lviv-define-prim 'log 1 'ln)
(define (expm1 x) (- (exp x) 1)) (lviv-define-prim 'expm1 1)
(define (lnp1 x) (+ (log x) 1)) (lviv-define-prim 'lnp1 1)
(define ln10 (log 10)) (define (log10 x) (/ (log x) ln10)) (lviv-define-prim 'log10 1 'log)
(define (alog x) (expt 10 x)) (lviv-define-prim 'alog 1)
(lviv-define-prim 'sin 1)
(lviv-define-prim 'cos 1)
(lviv-define-prim 'tan 1)
(define (sec x) (/ 1 (cos x))) (lviv-define-prim 'sec 1)
(define (csc x) (/ 1 (sin x))) (lviv-define-prim 'csc 1)
(define (cot x) (/ 1 (tan x))) (lviv-define-prim 'cot 1)
(lviv-define-prim 'asin 1)
(lviv-define-prim 'acos 1)
(lviv-define-prim 'atan 1)
(define (asec x) (acos (/ 1 x))) (lviv-define-prim 'asec 1)
(define (acsc x) (asin (/ 1 x))) (lviv-define-prim 'acsc 1)
(define (acot x) (atan (/ 1 x))) (lviv-define-prim 'acot 1)
(define (d>r x) (* pi (/ x 180))) (lviv-define-prim 'd>r 1)
(define (r>d x) (* 180 (/ x pi))) (lviv-define-prim 'r>d 1)
(lviv-define-prim '*atan 2 'atan2)
(define (vers x) (- 1 (cos x))) (lviv-define-prim 'vers 1)
(define (hav x) (/ (vers x) 2)) (lviv-define-prim 'hav 1)

; hyperbolic functions
(define (sinh x) (let ((expx (exp x))) (/ (- expx (inv expx)) 2)))
(lviv-define-prim 'sinh 1)
(define (cosh x) (let ((expx (exp x))) (/ (+ expx (inv expx)) 2)))
(lviv-define-prim 'cosh 1)
(define (tanh x) (let ((exp2x (exp (* 2 x)))) (/ (- exp2x 1) (+ exp2x 1))))
(lviv-define-prim 'tanh 1)
(define (asinh x) (log (+ x (sqrt (+ (sq x) 1)))))
(lviv-define-prim 'asinh 1)
(define (acosh x) (log (+ x (sqrt (- (sq x) 1)))))
(lviv-define-prim 'acosh 1)
(define (atanh x) (/ (- (log (+ 1 x)) (log (- 1 x))) 2))
(lviv-define-prim 'atanh 1)
(define (sech x) (inv (cosh x)))
(lviv-define-prim 'sech 1)
(define (csch x) (inv (sinh x)))
(lviv-define-prim 'csch 1)
(define (coth x) (inv (tanh x)))
(lviv-define-prim 'coth 1)
(define (asech x) (log (/ (+ 1 (sqrt (- 1 (sq x)))) x)))
(lviv-define-prim 'asech 1)
(define (acsch x) (log (+ (inv x) (/ (sqrt (+ 1 (sq x))) (abs x)))))
(lviv-define-prim 'acsch 1)
(define (acoth x) (/ (- (log (+ 1 (inv z))) (log (- 1 (inv z)))) 2))
(lviv-define-prim 'acoth 1)

; complex number functions
(lviv-define-prim 'magnitude 1 'mag)
(lviv-define-prim 'angle 1 'arg)
(lviv-define-prim 'imag-part 1 'im)
(lviv-define-prim 'real-part 1 're)
(lviv-define-prim 'make-rectangular 2 'cxrect)
(lviv-define-prim 'make-polar 2 'cxpolar)

; relational
(lviv-define-prim 'eq? 2)
(lviv-define-prim 'equal? 2)
(lviv-define-prim '= 2)
(lviv-define-prim '< 2)
(lviv-define-prim '> 2)
(lviv-define-prim '<= 2)
(lviv-define-prim '>= 2)
(define (andF y x) (and y x)) (lviv-define-prim 'andF 2 'and)
(define (orF y x) (or y x)) (lviv-define-prim 'orF 2 'or)
(define (xor y x) (or (and y (not x)) (and x (not y))))
(lviv-define-prim 'xor 2)
(lviv-define-prim 'not 1)

; misc
(define (fact x) (if (< x 1) 1 (* x (fact (- x 1)))))
(lviv-define-prim 'fact 1 '!)
(define (rnd y x) (/ (round (* y (alog x))) (alog x)))
(lviv-define-prim 'rnd 2)
(lviv-define-prim 'min 2)
(lviv-define-prim 'max 2)
(define (sign x) (cond ((< x 0) -1) ((> x 0) 1) (else 0)))
(lviv-define-prim 'sign 1)
(define (psign x) (cond ((< x 0) -1) (else 1)))
(lviv-define-prim 'psign 1)
(define (mant x) (number->int (/ (log (abs x)) ln10)))
(lviv-define-prim 'mant 1)
(define (xpon x) (/ x (expt 10 (mant x))))
(lviv-define-prim 'xpon 1)
(lviv-define-prim 'inexact->exact 1 'exact)
(lviv-define-prim 'exact->inexact 1 'inexact)
(lviv-define-prim 'random-integer 1 'randInt)
(lviv-define-prim 'random-real 0 'rand)
(define (perm y x)
  (cond ((> x y) 0)
        ((or (< x 1) (< y 1)) 0)
        (else
          (letrec
            ((tfact
               (lambda (n st)
                 (if (= n st) 1 (* n (tfact (- n 1) st))))))
            (tfact y (- y x))))))
(lviv-define-prim 'perm 2)
(define (comb y x) (/ (perm y x) (fact x)))
(lviv-define-prim 'comb 2)

; list manipulations
(lviv-define-prim 'append 2)
(lviv-define-prim 'cons 2)
(lviv-define-prim 'null? 1)

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

(add-cxrs lvivState 4)
(add-cxrs lvivState 3)
(add-cxrs lvivState 2)
(add-cxrs lvivState 1)

