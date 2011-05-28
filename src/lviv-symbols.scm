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

; ****************************
; **** SYMBOL DEFINITIONS ****
; ****************************
; all the crap with the #lviv# tags and whatnot

; lviv-tag is used to tag internal objects
(define lviv-tag '|(<#lviv#>)|)
(define (mklvivtag x) (cons lviv-tag x))
(define (lviv-tagged? x)
  (and (vector? x)
       (pair? (vector-ref x 0))
       (eq? lviv-tag (car (vector-ref x 0)))))

; illegal symbol is used to check static and quote symbols
; for legality. In particular, we care that the second character
; of the symbol isn't something illegal
(define illegal-chars (list #\. #\+ #\- #\& #\* #\!))
(define (illegal-symbol? symb)
  (member (string-ref (symbol->string symb) 1) illegal-chars))

(define elm? vector?)
(define mkElm vector)
(define (elmRef elm n) (vector-ref elm n))
(define elmLen vector-length)
(define elmCopy vector-copy)
(define (elmSet elm n val) (vector-set! elm n val))

; meta-test for symbolicity
; give it the symbol sigil and it
; returns the test function
(define (x-symbol? char)
  (lambda (symb)
    (and (symbol? symb) 
         (let ((symb-str (symbol->string symb)))
           (and (> (string-length symb-str) 1) ; '& is not a legal static symbol
                (eq? char (string-ref symb-str 0)))))))

; meta-converter
; give it the sigil and an error msg
; it returns the conversion function
(define (x-symbol->symbol symb)
  (let ((symb-str (symbol->string symb)))
    (string->symbol (substring symb-str 1 (string-length symb-str)))))

; meta-test for symbol-elementicity (that is, symbol in AST)
; give it symbol and length of representation
(define (x-symbol-elm? sym len)
  (lambda (elm)
    (and (elm? elm)
         (= len (elmLen elm))
         (equal? (mklvivtag sym)
                 (elmRef elm 0)))))

; static symbol functions
(define static-symbol? (x-symbol? #\&))
(define static-symbol->symbol x-symbol->symbol)
(define (mkStaticSymbolElm symb env)
  (if (illegal-symbol? symb) ; make sure it's legal
    (eLeft "illegal symbol") ; oops
    (mkElm (mklvivtag '&) (static-symbol->symbol symb) env)))
(define static-symbol-elm? (x-symbol-elm? '& 3))
(define (static-symbol-env elm) (elmRef elm 2))
(define (static-symbol-sn  elm) (object->serial-number
                                  (static-symbol-env elm)))
(define (static-symbol-sym elm) (elmRef elm 1))

; quote symbol functions
(define quote-symbol? (x-symbol? #\*))
(define (mkQuoteSymbolElm symb)
;  (if (illegal-symbol? symb) ; make sure it's legal
;    (eLeft "illegal symbol") ; oops
;***I think quotes should be OK as long as they parse
    (quote-symbol->symbol symb));)
(define quote-symbol->symbol x-symbol->symbol)
(define quote-symbol-elm? symbol?)

; is this symbol reversed?
(define reverse-symbol? (x-symbol? #\:))
(define reverse-symbol->symbol x-symbol->symbol)

; is this an element that can be used to define an environment variable?
(define (symbol-elm? item) (or (static-symbol-elm? item)
                               (quote-symbol-elm? item)))

; stackops in AST
(define (mkStackOpElm op name) (mkElm (mklvivtag 'stackop) op name))
(define stackOpElm? (x-symbol-elm? 'stackop 3))
(define (stackOpElm->stackop elm) (elmRef elm 1))
(define (stackOpElm-sym elm) (elmRef elm 2))

; thunks in AST
(define (mkThunkElm op)
  (mkElm (mklvivtag 'thunk)
         (if (list? op)
           op
           (list op))))
(define thunkElm? (x-symbol-elm? 'thunk 2))
(define (thunkElm->elm elm) (elmRef elm 1))

; make a lambda to stick in the env
(define (mkLambda code args env)
  (mkElm (mklvivtag 'lambda) args code env #f))
(define lambda? (x-symbol-elm? 'lambda 5))
; reverse order of application
(define (lambda-reverse binding)
  (let ((newElm (elmCopy binding)))
    (elmSet newElm 4 (not (lambda-reverse? binding)))
    newElm))
(define (lambda-args elm) (elmRef elm 1))
(define (lambda-code elm) (elmRef elm 2))
(define (lambda-env  elm) (elmRef elm 3))
(define (lambda-reverse? elm) (elmRef elm 4))

; make a primitive binding to stick in the env
(define (mkPrimBinding id arity)
  (mkElm (mklvivtag 'primitive) arity id #f))
(define primitive? (x-symbol-elm? 'primitive 4))
; change a binding to its reverse
(define (prim-reverse binding)
  (let ((newElm (elmCopy binding)))
    (elmSet newElm 3 (not (primitive-reverse? binding)))
    newElm))
; primitives
(define (primitive-arity elm) (elmRef elm 1))
(define (primitive-id elm) (elmRef elm 2))
(define (primitive-reverse? elm) (elmRef elm 3))

