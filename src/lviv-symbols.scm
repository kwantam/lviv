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
  (and (pair? x)
       (pair? (car x))
       (eq? lviv-tag (caar x))))

; illegal symbol is used to check static and quote symbols
; for legality. In particular, we care that the second character
; of the symbol isn't something illegal
(define illegal-chars (list #\. #\+ #\- #\& #\* #\!))
(define (illegal-symbol? symb)
  (member (string-ref (symbol->string symb) 1) illegal-chars))

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
    (and (list? elm)
         (= len (length elm))
         (equal? (mklvivtag sym) (car elm)))))

; static symbol functions
(define static-symbol? (x-symbol? #\&))
(define static-symbol->symbol x-symbol->symbol)
(define (mkStaticSymbolElm symb env)
  (if (illegal-symbol? symb) ; make sure it's legal
    (eLeft "illegal symbol") ; oops
    (list (mklvivtag '&) (static-symbol->symbol symb) (object->serial-number env))))
(define static-symbol-elm? (x-symbol-elm? '& 3))
(define (static-symbol-env symb) (serial-number->object (caddr symb)))
(define (static-symbol-sn symb) (caddr symb))
(define static-symbol-sym cadr)

; position symbol functions
(define posn-symbol? (x-symbol? #\!))
(define posn-symbol->symbol x-symbol->symbol)
(define (mkPosnRefElm symb)
  (let ((nNum (string->number 
                (list->string (cdr (string->list (symbol->string symb)))))))
    (if (not (and nNum
                  (= nNum (number->int nNum))
                  (>= nNum 0)))
      (eLeft "positional ref must be a non-negative integer")
      (list (mklvivtag '!) (posn-symbol->symbol symb)))))
(define posn-symbol-elm? (x-symbol-elm? '! 2))
(define posn-symbol-sym cadr)

; quote symbol functions
(define quote-symbol? (x-symbol? #\*))
(define (mkQuoteSymbolElm symb)
  (if (illegal-symbol? symb) ; make sure it's legal
    (eLeft "illegal symbol") ; oops
    (quote-symbol->symbol symb)))
(define quote-symbol->symbol x-symbol->symbol)
(define quote-symbol-elm? symbol?)

; is this symbol reversed?
(define reverse-symbol? (x-symbol? #\:))
(define reverse-symbol->symbol x-symbol->symbol)

; is this an element that can be used to define an environment variable?
(define (symbol-elm? item) (or (static-symbol-elm? item)
                               (quote-symbol-elm? item)))

; stackops in AST
(define (mkStackOpElm op name) (list (mklvivtag 'stackop) op name))
(define (stackOpElm? op)
  (and (list? op) (= (length op) 3) (equal? (mklvivtag 'stackop) (car op))))
(define stackOpElm->stackop cadr)
(define stackOpElm-sym caddr)

; thunks in AST
(define (mkThunkElm op)
  (cons (mklvivtag 'thunk)
        (if (list? op)
          op
          (list op))))
(define (thunkElm? op)
  (and (pair? op) (equal? (mklvivtag 'thunk) (car op))))
(define thunkElm->elm cdr)

; make a lambda to stick in the env
(define (mkLambda code args env)
  (list (mklvivtag 'lambda) args code (object->serial-number env) #f))
(define (lambda? obj)
  (and (list? obj) (= (length obj) 5) (equal? (car obj) (mklvivtag 'lambda))))
; reverse order of application
(define (lambda-reverse binding)
  (reverse (cons (not (lambda-reverse? binding)) (cdr (reverse binding)))))
(define (lambda-reverse? x) (list-ref x 4))
(define lambda-args cadr)
(define lambda-code caddr)
(define (lambda-env obj) (serial-number->object (car (cdddr obj))))

; make a primitive binding to stick in the env
(define (mkPrimBinding id arity)
  (list (mklvivtag 'primitive) arity id #f))

; change a binding to its reverse
(define (prim-reverse binding)
  (reverse (cons (not (primitive-reverse? binding)) (cdr (reverse binding)))))

; primitives
(define primitive-arity cadr)
(define primitive-id caddr)
(define primitive-reverse? cadddr)
(define (primitive? obj)
  (and (list? obj) (= (length obj) 4) (equal? (car obj) (mklvivtag 'primitive))))

