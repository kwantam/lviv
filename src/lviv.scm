#!/usr/bin/env gsi-script
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
(include "lviv-specforms.scm")
(include "lviv-exceptions.scm")
(include "lviv-symbols.scm")
(include "lviv-funcalls.scm")
(include "lviv-repl.scm")
(include "lviv-prelude.scm")
(include "lviv-tests.scm")

; go through each arg in the arglist
; - means run a repl
;
(define (lviv-process-args arglist)
  (cond ((null? arglist) #f)
        ((equal? "-" (car arglist))
         (lviv-repl lvivState '())
         (lviv-process-args (cdr arglist)))
        (else
          (lviv-file lvivState (car arglist))
          (lviv-process-args (cdr arglist)))))

; decide how to proceed based on commandline
; if a -- is supplied, ignore all args before it
; otherwise, attempt to open and eval all args
; other than the 0th
; this mimics the difference between script mode
; and batch mode in gsi
(let ((c--line (member "--" (command-line)))
      (c1line (cdr (command-line))))
  (cond ((null? c1line) ; no arguments at all
         (display "welcome to lviv\n\n")
         (lviv-repl lvivState '()))
        ((not c--line) ; didn't find -- delimiter
         (lviv-process-args c1line))
        ((null? (cdr c--line)) ; found --, if it's last arg just do repl
         (display "welcome to lviv\n\n")
         (lviv-repl lvivState '()))
        ; otherwise process args after --
        (else
          (lviv-process-args (cdr c--line)))))

; print the stack before we exit
(define (main . _) #f)
