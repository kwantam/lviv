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
; **************
; **** MISC ****
; **************
; miscellaneous functions

; foldl
; like the Haskell function
; foldl1 :: (a->b->a) -> a -> [b] -> a
(define (foldl f init ls)
  (if (null? ls)
    init
    (foldl f (f init (car ls)) (cdr ls))))

; zipWith
; like the Haskell function
; zipWith func l1 l2
(define (zipWith f l1 l2)
  (cond ((null? l1) '())
        ((null? l2) '())
        (else (cons (f (car l1) (car l2))
                    (zipWith f (cdr l1) (cdr l2))))))
; zip = zipWith cons
(define (zip l1 l2) (zipWith cons l1 l2))

; anyWith
; if the supplied test is true for any member
; of the list return #t, else return #f
(define (anyWith tst lst)
  (foldl (lambda (x y) (or x (tst y))) #f lst))

; allWith
; if the supplied test is false for any member
; of the list return #f, else return #t
(define (allWith tst lst)
  (foldl (lambda (x y) (and x (tst y))) #t lst))

; string-contains    s1 s2 [start1 end1 start2 end2] -> integer or false
; string-contains-ci s1 s2 [start1 end1 start2 end2] -> integer or false
;     Does string s1 contain string s2?
;     Return the index in s1 where s2 occurs as a substring, or false. The
;     optional start/end indices restrict the operation to the indicated
;     substrings.
; We do not support the optional arguments
; this function is from http://okmij.org/ftp/Scheme/lib/srfi-13-local.scm
; as noted on http://okmij.org/ftp/, this code is in the public domain
(define (string-contains str pattern)
  (let* ((pat-len (string-length pattern))
         (search-span (- (string-length str) pat-len))
         (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
         (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
      ((not c1) 0)           ; empty pattern, matches upfront
      ((not c2) (string-index str c1)) ; one-char pattern
      (else                  ; matching a pattern of at least two chars
        (let outer ((pos 0))
          (cond
            ((> pos search-span) #f)	; nothing was found thru the whole str
            ((not (char=? c1 (string-ref str pos)))
             (outer (+ 1 pos)))	; keep looking for the right beginning
            ((not (char=? c2 (string-ref str (+ 1 pos))))
             (outer (+ 1 pos)))	; could've done pos+2 if c1 == c2....
            (else                  	; two char matched: high probability
              ; the rest will match too
              (let inner ((i-pat 2) (i-str (+ 2 pos)))
                (if (>= i-pat pat-len) pos ; whole pattern matched
                  (if (char=? (string-ref pattern i-pat)
                              (string-ref str i-str))
                    (inner (+ 1 i-pat) (+ 1 i-str))
                    (outer (+ 1 pos))))))))))))	; mismatch after partial match

; same, but for a symbol via conversion to string
(define (symbol-contains k pstring)
  (string-contains (symbol->string k) pstring))

; take, like the Haskell function
; take :: Int -> [a] -> [a]
(define (take n lst)
  (cond ((= n 0) '())
        ((null? lst) '())
        (else (cons (car lst) (take (- n 1) (cdr lst))))))

; splitAt, like the Haskell function
; splitAt :: Int -> [a] -> ([a],[a])
(define (splitAt n lst)
  (letrec 
    ((splitAtHlp (lambda (lst n hd)
                   (cond ((null? lst) (cons (reverse hd) '())) ; no more list to split
                         ((<= n 0) (cons (reverse hd) lst))
                         (else 
                           (splitAtHlp (cdr lst) (- n 1) (cons (car lst) hd)))))))
    (splitAtHlp lst n '())))

; like iterate f lst !! x
(define (iterateN f n lst)
  (if (= n 0) lst (iterateN f (- n 1) (f lst))))

; iterate, but quit on empty list
(define (iterateNOrNull f n lst)
  (if (or (= n 0) (null? lst)) lst (iterateNOrNull f (- n 1) (f lst))))

; general number to integer
(define (number->int num) (inexact->exact (floor num)))

; rollN circular shifts the first few elements of a stack
; first element becomes second, et cetera
(define (rollN n lst)
  (let ((splitLst (splitAt (- (min n (length lst)) 1) lst)))
    (cons (cadr splitLst) (append (car splitLst) (cddr splitLst)))))

; unrollN circular shifts like rollN, but the other direction
(define (unrollN n lst)
  (let ((splitLst (splitAt (min n (length lst)) lst)))
    (append (cdar splitLst) (cons (caar splitLst) (cdr splitLst)))))

; dupN duplicates the first n elements of a list
(define (dupN n lst)
  (let ((splitLst (splitAt (min n (length lst)) lst)))
    (append (car splitLst) (append (car splitLst) (cdr splitLst)))))

; flip function of two variables
(define (flip f) (lambda (x y) (f y x)))

; pickN - like a consing list-ref but uses 1-based indexing
; attempts to never produce an error
(define (pickN n lst)
  (cond ((< n 1) lst)
        ((> n (length lst)) lst)
        (else (cons (list-ref lst (- n 1)) lst))))

; eLeft and eRight are like the Either monad
; eRight signals success, eLeft signals failure
(define (eLeft msg) (if (eLeft? msg) msg (cons '(either #f) msg)))
(define (eRight msg) (if (eRight? msg) msg (cons '(either #t) msg)))
(define (eRight? either) (and (pair? either) (equal? '(either #t) (car either))))
(define (eLeft? either) (and (pair? either) (equal? '(either #f) (car either))))
(define fromLeftRight cdr)

; strict truth and falsity tests
(define (=bool? b)
  (lambda (x)
    (if (not (boolean? x))
      (raise "type error")
      (eq? b x))))
(define =true? (=bool? #t))
(define =false? (=bool? #f))


