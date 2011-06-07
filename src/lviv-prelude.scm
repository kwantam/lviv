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

(define (symbol->quote-symbol symb)
  (string->symbol (string-append "*" (symbol->string symb))))

(define (lviv-define-prim symb arity . name)
  (let ((qsymb (symbol->quote-symbol symb)))
    (if (null? name)
      (applyMap lvivState (quasiquote (,arity ,qsymb primitive ,qsymb define)))
      (let ((cName (symbol->quote-symbol (car name))))
        (applyMap lvivState (quasiquote (,arity ,qsymb primitive ,cName define)))))))

(define (lviv-define-val symb val)
  (applyMap lvivState (quasiquote (,val ,(symbol->quote-symbol symb) define))))

; constants
(define pi 3.141592653589793238462643) ; more than we can actually represent
(define pi/2 (/ pi 2))
(lviv-define-val 'pi pi)
(lviv-define-val 'pi/2 pi/2)
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

(define n1toN (fromTo 1 100)) ; waaaaaaaaaay overkill
(define invFacts (scanl (lambda (y x) (/ y x)) 1 n1toN))
(define invEONeg (map (lambda (x) (/ (if (even? x) -1 1) x)) n1toN))
(define m1^k (map (lambda (x) (if (even? x) 1 -1)) n1toN))

(define (expm1 x) ; use taylor expansion of e^x near 0 to reduce numerical error
  (if (< (magnitude x) 0.19)
    (letrec ((expm1Hlp
               (lambda (expts iFacts)
                 (if (null? expts) 0
                   (+ (* (expt x (car expts)) (car iFacts))
                      (expm1Hlp (cdr expts) (cdr iFacts)))))))
      (+ x (expm1Hlp (cdr n1toN) (cdr invFacts))))
    (- (exp x) 1)))
(lviv-define-prim 'expm1 1)

(define (lnp1 x) ; use taylor expansion of ln(1+x) near 0 to reduce numerical error
  (if (< (magnitude x) 0.19)
    (letrec ((lnp1Hlp
               (lambda (expts quots)
                 (if (null? expts) 0
                   (+ (* (expt x (car expts)) (car quots))
                      (lnp1Hlp (cdr expts) (cdr quots)))))))
      (+ x (lnp1Hlp (cdr n1toN) (cdr invEONeg))))
    (log (+ x 1))))
(lviv-define-prim 'lnp1 1)

(define ln10 (log 10)) (define (log10 x) (/ (log x) ln10)) (lviv-define-prim 'log10 1 'log)
(define (alog x) (expt 10 x)) (lviv-define-prim 'alog 1)
(lviv-define-prim 'sin 1)
(lviv-define-prim 'cos 1)
(lviv-define-prim 'tan 1)
(define (sec x) (/ 1 (cos x))) (lviv-define-prim 'sec 1)
(define (csc x)
  (if (= x 0) +inf.0 (/ 1 (sin x)))) (lviv-define-prim 'csc 1)
(define (cot x)
  (if (= x 0) +inf.0 (/ 1 (tan x)))) (lviv-define-prim 'cot 1)
(lviv-define-prim 'asin 1)
(lviv-define-prim 'acos 1)
(lviv-define-prim 'atan 1)
(define (asec x)
  (if (= 0 x) +inf.0i (acos (/ 1 x)))) (lviv-define-prim 'asec 1)
(define (acsc x)
  (if (= 0 x) (- pi/2 +inf.0i) (asin (/ 1 x)))) (lviv-define-prim 'acsc 1)
(define (acot x)
  (if (= 0 x) pi/2 (atan (/ 1 x)))) (lviv-define-prim 'acot 1)
(define (d>r x) (* pi (/ x 180))) (lviv-define-prim 'd>r 1)
(define (r>d x) (* 180 (/ x pi))) (lviv-define-prim 'r>d 1)
(lviv-define-prim 'atan 2 'atan2)
(define (vers x) (- 1 (cos x))) (lviv-define-prim 'vers 1)
(define (hav x) (/ (vers x) 2)) (lviv-define-prim 'hav 1)

; bernouilli numbers for estimating tanh
; ** sinh(x)/cosh(x) is sufficiently accurate
; ** so we'll just do that instead
; ** note: the following algorithm is basically a direct
; implementation of the Akiyama-Tanigawa triangle from
; http://www.cs.uwaterloo.ca/journals/JIS/VOL3/KANEKO/AT-kaneko.pdf
;(define maxBN 101)
;(define n1toNBN (fromTo 1 maxBN))
;(define bnL (list (map (lambda (x) (/ 1 x)) n1toNBN)))
;(set-cdr! bnL (cons (cdar bnL) '()))
;(define (updateNthBN n bn)
;  (if (< (length bn) n) (raise "need predecessor")
;    (let ((bncdr (iterateN cdr (- n 1) bn)))
;      (set-cdr! bncdr
;                (cons (zipWith * n1toNBN 
;                               (zipWith - (car bncdr) (cdar bncdr)))
;                      '())))))
;(map (lambda (x) (updateNthBN x bnL)) (cdr n1toNBN))
;(define bNums (reverse (map car (cdr (reverse bnL)))))
(define (everyOther ls)
  (if (or (null? ls) (null? (cdr ls)))
    '()
    (cons (car ls) (everyOther (cddr ls)))))
;(define tanhFacts
;  (zipWith
;    * (everyOther (cddr bNums))
;    (zipWith (lambda (x y) (* (- (expt 4 x) (expt 2 x))
;                              y))
;             (everyOther (cdr n1toNBN))
;             (everyOther (cdr invFacts)))))

; hyperbolic functions
(define (sinh x)
  (if (< (magnitude x) 0.19)
    (letrec ((sinhHlp
               (lambda (expts iFacts)
                 (if (or (null? expts) (null? (cdr expts))) 0
                   (+ (* (expt x (car expts)) (car iFacts))
                      (sinhHlp (cddr expts) (cddr iFacts)))))))
      (+ x (sinhHlp (cddr n1toN) (cddr invFacts))))
    (let ((expx (exp x))) (/ (- expx (inv expx)) 2))))
(lviv-define-prim 'sinh 1)
(define (cosh x)
  (if (< (magnitude x) 0.19) 
    (letrec ((coshHlp
               (lambda (expts iFacts)
                 (if (or (null? expts) (null? (cdr expts))) 0
                   (+ (* (expt x (car expts)) (car iFacts))
                      (coshHlp (cddr expts) (cddr iFacts)))))))
      (+ 1 (coshHlp (cdr n1toN) (cdr invFacts))))
    (let ((expx (exp x))) (/ (+ expx (inv expx)) 2))))
(lviv-define-prim 'cosh 1)

;(define (tanh x)
;  (if (< (magnitude x) 0.19)
;    (letrec ((tanhHlp
;               (lambda (ks xps)
;                 (if (null? ks) 0
;                   (+ (* (car ks) (expt x (car xps)))
;                      (tanhHlp (cdr ks) (cddr xps)))))))
;      (+ x (tanhHlp (cdr tanhFacts) (cddr n1toN))))
;    (let ((exp2x (exp (* 2 x)))) (/ (- exp2x 1) (+ exp2x 1)))))

; this is for the case where sinh(x) and cosh(x) blow up to +inf.0
; we know that in this case they blow up in such a way that the
; correct answer is just 1
(define (tanh x) 
  (let ((sinhx (sinh x))
        (coshx (cosh x)))
    (if (= sinhx coshx) 1
        (/ sinhx coshx))))
(lviv-define-prim 'tanh 1)

(define acoshTArgs
  (zipWith * (everyOther (cddr invFacts))
           (map sq (scanl * 1 (everyOther n1toN)))))
(define asinhTArgs
  (zipWith * acoshTArgs m1^k))
(define (asinh x)
  (if (< (magnitude x) 0.35)
    (letrec ((asinhHlp
               (lambda (xp ta)
                 (if (null? ta) 0
                   (+ (* (expt x (car xp)) (car ta))
                      (asinhHlp (cddr xp) (cdr ta)))))))
      (+ x (asinhHlp (cddr n1toN) asinhTArgs)))
    (log (+ x (sqrt (+ (sq x) 1))))))
(lviv-define-prim 'asinh 1)

(define (acosh x)
  (if (< (magnitude x) 0.75)
    (letrec ((acoshHlp
               (lambda (xp ta)
                 (if (null? ta) 0
                   (+ (* (expt x (car xp)) (car ta))
                      (acoshHlp (cddr xp) (cdr ta)))))))
      (+ (log -i)
         (* +i (+ x (acoshHlp (cddr n1toN) acoshTArgs)))))
    (log (+ x (sqrt (- (sq x) 1))))))
(lviv-define-prim 'acosh 1)

(define (atanh x) (/ (- (lnp1 x) (lnp1 (chs x))) 2))
(lviv-define-prim 'atanh 1)

(define (sech x) (inv (cosh x)))
(lviv-define-prim 'sech 1)
(define (csch x) (inv (sinh x)))
(lviv-define-prim 'csch 1)
(define (coth x) (inv (tanh x)))
(lviv-define-prim 'coth 1)

(define (asech x)
  (if (= x 0) +inf.0 (log (/ (+ 1 (sqrt (- 1 (sq x)))) x))))
(lviv-define-prim 'asech 1)

(define (acsch x)
  (if (= x 0) +inf.0 (log (+ (inv x) (/ (sqrt (+ 1 (sq x))) (magnitude x))))))
(lviv-define-prim 'acsch 1)

(define mipi/2 (acosh 0))
(define (acoth x)
  (if (< (magnitude x) 0.19)
    ((if (< x 0) - +) (atanh x) mipi/2)
    (/ (- (lnp1 (inv x)) (lnp1 (chs (inv x)))) 2)))
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
(define (xpon x)
  (cond ((zero? x) 0)
        ((and (real? x) (nan? x)) +nan.0)
        ((and (real? x) (infinite? x)) 0)
        ((and (complex? x) 
              (let ((imagx (imag-part x))
                    (realx (real-part x)))
                (or (infinite? imagx)
                    (nan? imagx)
                    (infinite? realx)
                    (nan? realx))))
         0)
        (else (inexact->exact (floor (/ (log (magnitude x)) ln10))))))
(lviv-define-prim 'xpon 1)
(define (mant x)
  (if (zero? x)
    0 (/ x (expt 10 (xpon x)))))
(lviv-define-prim 'mant 1)
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
(lviv-define-prim 'reverse 1)

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
