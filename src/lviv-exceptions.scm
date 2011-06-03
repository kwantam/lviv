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

; ############################
; #### EXCEPTION HANDLING ####
; ############################

; rewind is an exception object that we raise
; when we want to undo some effect on the stack
(define (rewind state stackArgs msg)
  (raise (list 'rewind state stackArgs msg)))

(define (rewind? exc)
  (and (list? exc) (eq? (car exc) 'rewind) (= (length exc) 4)))

(define (stackError result)
  (raise (list 'stackError (fromLeftRight result))))

(define (stackError? exc)
  (and (list? exc) 
       (= (length exc) 2)
       (eq? (car exc) 'stackError)
       (string? (cadr exc))))

(define (symbErr symb err)
  (raise (string-append (symbol->string symb)
                        ": "
                        err)))

(define (dispErr_ msg)
  (let ((errMsg (string-append "--> error: " msg "\n")))
    (display errMsg)
    (eLeft msg)))

; we want to provide reasonable exceptions to the user, so we do our best
; to catch what's coming from the interpreter and turn it into something
; intelligible
(define (exceptionHandler display?)
  (lambda (exc)
  (let ((dispErr (if display? dispErr_ eLeft)))
  (cond ((rewind? exc)
         (let ((state (cadr exc))
               (stackArgs (caddr exc))
               (msg (cadddr exc)))
           (stStackNPush state stackArgs)
           (dispErr msg)))
        ((stackError? exc)
         (dispErr (cadr exc)))
        ((noncontinuable-exception? exc)
         (dispErr (noncontinuable-exception-reason exc)))
        ((heap-overflow-exception? exc)
         (dispErr "heap overflow"))
        ((stack-overflow-exception? exc)
         (dispErr "call stack overflow"))
        ((os-exception? exc)
         (dispErr (os-exception-message exc)))
        ((no-such-file-or-directory-exception? exc)
         (dispErr "no such file or directory"))
        ((unbound-os-environment-variable-exception? exc)
         (dispErr "unbound env variable"))
        ((scheduler-exception? exc)
         (dispErr "scheduler exception"))
        ((deadlock-exception? exc)
         (dispErr "deadlock exception"))
        ((abandoned-mutex-exception? exc)
         (dispErr "abandoned mutex"))
        ((join-timeout-exception? exc)
         (dispErr "join timeout"))
        ((started-thread-exception? exc)
         (dispErr "thread started"))
        ((terminated-thread-exception? exc)
         (dispErr "thread terminated"))
        ((uncaught-exception? exc)
         (dispErr "uncaught exception"))
        ((cfun-conversion-exception? exc)
         (dispErr "C function exception"))
        ((sfun-conversion-exception? exc)
         (dispErr "Sfun exception"))
        ((multiple-c-return-exception? exc)
         (dispErr "multiple C return"))
        ((datum-parsing-exception? exc)
         (dispErr "bad read"))
        ((expression-parsing-exception? exc)
         (dispErr "bad parse"))
        ((unbound-global-exception? exc) 
         (dispErr (string-append
                  "unbound global exception: "
                  (symbol->string
                    (unbound-global-exception-variable exc)))))
        ((type-exception? exc)
         (dispErr "type exception"))
        ((range-exception? exc)
         (dispErr "range exception"))
        ((improper-length-list-exception? exc)
         (dispErr "improper length list"))
        ((wrong-number-of-arguments-exception? exc)
         (dispErr "wrong number of arguments"))
        ((number-of-arguments-limit-exception? exc)
         (dispErr "number of arguments limit"))
        ((nonprocedure-operator-exception? exc)
         (dispErr "nonprocedure operator"))
        ((unknown-keyword-argument-exception? exc)
         (dispErr "unknown keyword argument"))
        ((keyword-expected-exception? exc)
         (dispErr "keyword expected"))
        ((error-exception? exc)
         (dispErr (string-append "error: " 
                                 (if (string? (error-exception-message exc))
                                   (error-exception-message exc)
                                   "error exception raised"))))
        ((divide-by-zero-exception? exc)
         (dispErr "divide by zero"))
        ((string? exc)
         (dispErr exc))
        (else
          (dispErr "unknown exception"))))))

(define exceptionHandlerPrint (exceptionHandler #t))
(define exceptionHandlerQuiet (exceptionHandler #f))

