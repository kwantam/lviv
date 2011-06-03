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
(define illegal-chars (list #\& #\* #\. #\+ #\-))
(define (illegal-symbol? symb)
  (member (string-ref (symbol->string symb) 1) illegal-chars))
(define (illegal-quote-symbol? symb)
  (member (string-ref (symbol->string symb) 1) (cddr illegal-chars)))

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
           (and (eq? char (string-ref symb-str 0))
                (> (string-length symb-str) 1))))))

; like x-symbol?, but without
; symbolicity check for faster
; eval
(define (x-symbol-unchecked? char)
  (lambda (symb)
    (let ((symb-str (symbol->string symb)))
      (and (eq? char (string-ref symb-str 0))
           (> (string-length symb-str) 1)))))

; meta-converter
; give it the sigil and an error msg
; it returns the conversion function
(define (x-symbol->symbol symb)
  (let ((symb-str (symbol->string symb)))
    (string->symbol (substring symb-str 1 (string-length symb-str)))))

; meta-test for symbol-elementicity (that is, symbol in AST)
; give it symbol and length of representation
(define (x-symbol-elm? symTag len)
  (lambda (elm)
    (and (elm? elm)
         (equal? symTag
                 (elmRef elm 0))
         (= len (elmLen elm)))))

; like x-symbol-elm? but without
; the elm? check for faster apply
(define (x-symbol-elm-unchecked? symTag len)
  (lambda (elm)
    (and (equal? symTag
                 (elmRef elm 0))
         (= len (elmLen elm)))))

; static symbol functions
(define static-symbol? (x-symbol? #\&))
(define static-symbol-unchecked? (x-symbol-unchecked? #\&))
(define static-symbol->symbol x-symbol->symbol)
(define staticLvivTag (mklvivtag '&))
(define (mkStaticSymbolElm symb env)
  (if (illegal-symbol? symb) ; make sure it's legal
    (eLeft "illegal symbol") ; oops
    (mkElm staticLvivTag (static-symbol->symbol symb) env)))
(define static-symbol-elm? (x-symbol-elm? staticLvivTag 3))
(define (static-symbol-env elm) (elmRef elm 2))
(define (static-symbol-sn  elm) (object->serial-number
                                  (static-symbol-env elm)))
(define (static-symbol-sym elm) (elmRef elm 1))

; quote symbol functions
(define quote-symbol? (x-symbol? #\*))
(define quote-symbol-unchecked? (x-symbol-unchecked? #\*))
(define (mkQuoteSymbolElm symb)
  (or (and (symbol? symb) (quote-symbol->symbol symb)) ; make sure it's legal
      (eLeft "illegal quote symbol")))                 ; otherwise error
(define quote-symbol->symbol x-symbol->symbol)
(define quote-symbol-elm? symbol?)

; is this symbol reversed?
(define reverse-symbol? (x-symbol? #\:))
(define reverse-symbol-unchecked? (x-symbol-unchecked? #\:))
(define reverse-symbol->symbol x-symbol->symbol)

; is this an element that can be used to define an environment variable?
(define (symbol-elm? item) (or (static-symbol-elm? item)
                               (quote-symbol-elm? item)))

; stackops in AST
(define stackopLvivTag (mklvivtag 'stackop))
(define (mkStackOpElm op name) (mkElm stackopLvivTag op name))
(define stackOpElm? (x-symbol-elm? stackopLvivTag 3))
(define stackOpElm-unchecked? (x-symbol-elm-unchecked? stackopLvivTag 3))
(define (stackOpElm->stackop elm) (elmRef elm 1))
(define (stackOpElm-sym elm) (elmRef elm 2))

; thunks in AST
(define thunkLvivTag (mklvivtag 'thunk))
(define (mkThunkElm op)
  (mkElm thunkLvivTag
         (if (list? op)
           op
           (list op))))
(define thunkElm? (x-symbol-elm? thunkLvivTag 2))
(define (thunkElm->elm elm) (elmRef elm 1))

; make a lambda to stick in the env
(define lambdaLvivTag (mklvivtag 'lambda))
(define (mkLambda code args env)
  (mkElm lambdaLvivTag args code env #f))
(define lambda? (x-symbol-elm? lambdaLvivTag 5))
(define lambda-unchecked? (x-symbol-elm-unchecked? lambdaLvivTag 5))
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
(define primitiveLvivTag (mklvivtag 'primitive))
(define (mkPrimBinding id arity)
  (mkElm primitiveLvivTag arity id #f))
(define primitive? (x-symbol-elm? primitiveLvivTag 4))
(define primitive-unchecked? (x-symbol-elm-unchecked? primitiveLvivTag 4))
; change a binding to its reverse
(define (prim-reverse binding)
  (let ((newElm (elmCopy binding)))
    (elmSet newElm 3 (not (primitive-reverse? binding)))
    newElm))
; primitives
(define (primitive-arity elm) (elmRef elm 1))
(define (primitive-id elm) (elmRef elm 2))
(define (primitive-reverse? elm) (elmRef elm 3))

