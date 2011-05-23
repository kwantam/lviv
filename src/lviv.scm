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
; **************
; **** MISC ****
; **************

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

; ***************
; **** STATE ****
; ***************

; an empty stack
(define mkEmptyStack (lambda () '()))

; an empty environment
(define mkEmptyEnv (lambda () (cons '() '())))

; an interpreter state is just (stack . env)
(define mkEmptyState (lambda () (cons (mkEmptyStack) (mkEmptyEnv))))

; retrieve stack from the state
(define stGetStack car)

; retrieve env from the state
(define stGetEnv cdr)

; update environment
(define (stUpdateEnv oldState newEnv) (set-cdr! oldState newEnv))
(define (stUpdateStack oldState newStack) (set-car! oldState newStack))

; ***************
; **** STACK ****
; ***************

; update stack, returning a value
; supplied function should return a value such that
; result is (car (f stack))
; newStk is (cdr (f stack))
(define (stStackUpd2 f state)
  (let* ((fValue (f (stGetStack state)))
         (fResult (car fValue))
         (fRemain (cdr fValue)))
    (stUpdateStack state fRemain)
    fResult))

; update stack, returning new stack
; cannot return an error
(define (stStackUpd f state)
  (let ((fValue (f (stGetStack state))))
    (stUpdateStack state fValue)
    (eRight '())))

; push onto stack
(define (stackPush var)
  (lambda (stack) (cons var stack)))
(define (stStackPush state var) 
  (stStackUpd (stackPush var) state))

; push a list of values back onto the stack
(define (stackNPush var)
  (lambda (stack) (append var stack)))
(define (stStackNPush state var)
  (stStackUpd (stackNPush var) state))

; stack depth
(define depth length)
(define (stStackDepth state) (depth (stGetStack state)))

; pop off stack
; when popping, return (cons <popped value> <new stack>)
; since we represent the stack as a list, pop is just
; the identity function
(define (stackPop stack)
  (if (> (depth stack) 0)
    (cons (eRight (car stack)) (cdr stack))
    (cons (eLeft "pop: stack empty") '())))

(define (stStackPop state) (stStackUpd2 stackPop state))

; pop n items off stack
; this is used when executing functions
(define (stackPopN n)
  (lambda (stack)
    (if (< (depth stack) n)
      (cons (eLeft "popN: not enough arguments") stack)
      (letrec ((sPopNHlp
                 (lambda (stack accum n)
                   (if (= n 0)
                     (cons (eRight accum) stack)
                     (sPopNHlp (cdr stack) (cons (car stack) accum) (- n 1))))))
        (sPopNHlp stack '() n)))))
(define (stStackNPop state n) (stStackUpd2 (stackPopN n) state))

; swap the 0th and 1st elements of the stack
(define (stackSwap stack)
  (if (> (depth stack) 1)
    (cons (eRight '()) (cons (cadr stack) (cons (car stack) (cddr stack))))
    (cons (eLeft "swap: not enough elements") stack)))
(define (stStackSwap state) (stStackUpd2 stackSwap state))

; drop the 0th element
(define (stackDrop stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cdr stack))
    (cons (eLeft "drop: stack empty") '())))
(define (stStackDrop state) (stStackUpd2 stackDrop state))

; clear the stack, i.e., replace it with emptyState
(define stackClear (lambda (x) (mkEmptyStack)))
(define (stStackClear state) (stStackUpd stackClear state))

; a generalized stack operation that takes the 0th elem
; off the stack, expecting an integer, and produces a
; modified stack as a result
(define (stackOpN f)
  (lambda (stack)
    (let* ((popVal (stackPop stack))
           (popEither (car popVal))
           (popRes (fromLeftRight popEither))
           (popRem (cdr popVal)))
      (cond ((eLeft? popEither) popVal)
            ((number? popRes)
             (if (<= (number->int popRes) (depth popRem))
               (cons (eRight '()) (f (number->int popRes) popRem))
               (cons (eLeft "stackOpN: not enough elements") stack)))
            (else (cons (eLeft "stackOpN: non-numeric argument") stack))))))

; drop N elements after the 0th
(define stackDropN
  (stackOpN (lambda (n st) (iterateNOrNull cdr n st))))
(define (stStackDropN state) (stStackUpd2 stackDropN state))

; roll the top N elements after the 0th
(define stackRollN (stackOpN rollN))
(define (stStackRollN state) (stStackUpd2 stackRollN state))

; unroll the top N elements after the 0th
(define stackUnrollN (stackOpN unrollN))
(define (stStackUnrollN state) (stStackUpd2 stackUnrollN state))

; dup the top element
(define (stackDup stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cons (car stack) stack))
    (cons (eLeft "dup: stack empty") '())))
(define (stStackDup state) (stStackUpd2 stackDup state))

; dup the first N elements after the 0th
(define stackDupN (stackOpN dupN))
(define (stStackDupN state) (stStackUpd2 stackDupN state))

; duplicate the second element on the stack
(define (stackOver stack)
  (if (> (depth stack) 1)
    (cons (eRight '()) (cons (cadr stack) stack))
    (cons (eLeft "over: not enough elements") stack)))
(define (stStackOver state) (stStackUpd2 stackOver state))

; duplicate the nth element on the stack
(define stackPickN (stackOpN pickN))
(define (stStackPickN state) (stStackUpd2 stackPickN state))

; generalized stack operation that takes the 0th elem
; off the stack and evaluates it for truth. If true,
; a supplied stackop is executed as long as there are
; sufficient elements in the stack
(define (stackOpBool bool f)
  (lambda (stack)
    (let* ((popVal (stackPop stack))
           (popEither (car popVal))
           (popRes (fromLeftRight popEither))
           (popRem (cdr popVal)))
      (with-exception-catcher
        (lambda (x) (cons (eLeft "type error") stack))
        (lambda ()
          (cond ((eLeft? popEither) popVal)
                ((bool popRes) (f popRem))
                (else (cons (eRight '()) popRem))))))))

; swapIf
(define stackSwapIf
  (stackOpBool =true? stackSwap))
(define (stStackSwapIf state) (stStackUpd2 stackSwapIf state))

; swapUnless
(define stackSwapUnless
  (stackOpBool =false? stackSwap))
(define (stStackSwapUnless state) (stStackUpd2 stackSwapUnless state))

; dropIf
(define stackDropIf
  (stackOpBool =true? stackDrop))
(define (stStackDropIf state) (stStackUpd2 stackDropIf state))

; dropUnless
(define stackDropUnless
  (stackOpBool =false? stackDrop))
(define (stStackDropUnless state) (stStackUpd2 stackDropUnless state))

; uncons
; if the first element of the stack is a list, replace it
; with car and cdr of list in 0th and 1st positions, respectively
(define (stackUncons stack)
  (let* ((popVal (stackPop stack))
         (popEither (car popVal))
         (popRes (fromLeftRight popEither))
         (popRem (cdr popVal)))
    (cond ((eLeft? popEither) popVal)
          ((not (pair? popRes)) (cons (eLeft "not a pair") stack))
          (else (cons (eRight '()) 
                      (append (list (car popRes) (cdr popRes))
                              popRem))))))
(define (stStackUncons state) (stStackUpd2 stackUncons state))

; a thunk is a piece of code that is idempotent through eval
; and requires an apply to get "opened up"
; applying a thunk is like evaling its contents
(define (stackThunk stack)
  (if (> (depth stack) 0)
    (cons (eRight '()) (cons (mkThunkElm (car stack)) (cdr stack)))
    (cons (eLeft "thunk: stack empty"))))
(define (stStackThunk state) (stStackUpd2 stackThunk state))

(define (stackOp? symb f)
  (lambda (op) (if (eq? symb op) f #f)))

; stackop lookup
(define (stStackOp? op)
  (or ((stackOp? 'depth stStackDepth) op)
      ((stackOp? 'swap stStackSwap) op)
      ((stackOp? 'drop stStackDrop) op)
      ((stackOp? 'clear stStackClear) op)
      ((stackOp? 'dropN stStackDropN) op)
      ((stackOp? 'roll stStackRollN) op)
      ((stackOp? 'unroll stStackUnrollN) op)
      ((stackOp? 'dup stStackDup) op)
      ((stackOp? 'dupN stStackDupN) op)
      ((stackOp? 'over stStackOver) op)
      ((stackOp? 'pick stStackPickN) op)
      ((stackOp? 'swapIf stStackSwapIf) op)
      ((stackOp? 'swapUnless stStackSwapUnless) op)
      ((stackOp? 'dropIf stStackDropIf) op)
      ((stackOp? 'dropUnless stStackDropUnless) op)
      ((stackOp? 'uncons stStackUncons) op)
      ((stackOp? 'define stDefine) op)
      ((stackOp? 'eval stEval) op)
      ((stackOp? 'apply stApply) op)
      ((stackOp? 'thunk stStackThunk) op)))

; #############
; #### ENV #### 
; #############

; env bindings
(define envBindings car)
(define envFirstBinding caar)
(define envRemBindings cdar)
(define envSetBindings set-car!)
(define firstBinding car)
(define restBindings cdr)
(define rRestBindings cddr)
(define setRestBindings set-cdr!)
; get env parent
(define envParent cdr)
; no updateParent necessary, we never rebase an env

; is this the global env?
(define (stGlobalEnv? state) (null? (envParent (stGetEnv state))))

; make a new child env from the parent
(define (envNewChild env) (cons '() env))
(define (stEnvNewChild state) (stUpdateEnv state (envNewChild (stGetEnv state))))

(define (stEnvParent state)
  (if (not (stGlobalEnv? state))
    (stUpdateEnv state (envParent (stGetEnv state)))))

; convert an envBinding operation to a stEnvBinding operation
(define (stEnvBindOp f)
  (lambda (state item) (f (stGetEnv state) item)))

; delete binding from env
(define (envDelBinding env name)
  (letrec ((delHlp
             (lambda (bindings)
               (cond ((null? (restBindings bindings)) #f)
                     ((eq? name (car (firstBinding (restBindings bindings))))
                      (setRestBindings bindings (rRestBindings bindings)))
                     (else (delHlp (restBindings bindings)))))))
    (cond ((null? (envBindings env)) #f)
          ((eq? name (car (firstBinding (envBindings env))))
           (envSetBindings env (restBindings (envBindings env))))
          (else (delHlp (envBindings env))))))
(define stEnvDelBinding (stEnvBindOp envDelBinding))

; insert an item into the environment
; does not check whether item is already there
(define (envAddBinding env item)
  (envSetBindings env (cons item (envBindings env))))

; update or insert binding into env
(define (envUpdateBinding env item)
  (letrec ((updateHlp
             (lambda (bindings) 
               (cond ((null? bindings) (envAddBinding env item))
                     ((eq? (car item) (car (firstBinding bindings)))
                      (set-cdr! (firstBinding bindings) (cdr item)))
                     (else (updateHlp (restBindings bindings)))))))
    (updateHlp (envBindings env))))
(define stEnvUpdateBinding (stEnvBindOp envUpdateBinding))

(define (envLookupBinding env name)
  (if (null? env)
    (eLeft "not found")
    (letrec ((lookupHlp
               (lambda (bindings)
                 (cond ((null? bindings) (envLookupBinding (envParent env) name))
                       ((eq? name (car (firstBinding bindings)))
                        (eRight (cdr (firstBinding bindings))))
                       (else (lookupHlp (restBindings bindings)))))))
      (lookupHlp (envBindings env)))))
(define stEnvLookupBinding (stEnvBindOp envLookupBinding))

(define (stDefine state)
  (let* ((fnIdE (stStackPop state))
         (fnId (fromLeftRight fnIdE))
         (fnVal (delay (stStackPop state)))
         (saEnvItem
           (delay (cons (static/auto-symbol-sym fnId)
                        (fromLeftRight (force fnVal)))))
         (envItem
           (delay (cons fnId
                        (fromLeftRight (force fnVal))))))
    (cond ((eLeft? fnIdE) fnIdE)
          ((not (or (static-symbol-elm? fnId) (symbol? fnId) (auto-symbol-elm? fnId)))
           (rewind state (list fnId) "invalid identifier"))
          ((eLeft? (force fnVal))
           (rewind state (list fnId) (fromLeftRight (force fnVal))))
          ((static-symbol-elm? fnId)
           (eRight
             (envUpdateBinding (static-symbol-env fnId)
                               (force saEnvItem))))
          ((auto-symbol-elm? fnId)
           (eRight (stEnvUpdateBinding myState (force saEnvItem))))
          (else
           (eRight (stEnvUpdateBinding myState (force envItem)))))))

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
  (and (list? exc) (eq? (car exc) 'stackError) (= (length exc) 2)))

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
        (else
          (dispErr "unknown exception"))))))

; ########################
; #### FUNCTION CALLS ####
; ########################

; lviv-tag is used to tag internal objects
(define lviv-tag '|(<#lviv#>)|)
(define (mklvivtag x) (list lviv-tag x))
(define (lviv-tagged? x)
  (and (pair? x)
       (pair? (car x))
       (eq? lviv-tag (caar x))))

; make a primitive binding to stick in the env
(define (mkPrimBinding id arity)
  (list (mklvivtag 'primitive) arity id #f))

; make a primitive binding that causes its arguments
; to be applied in reverse
; `:func` is the reverse version of `:func`
(define (mkRPrimBinding id arity)
  (list (mklvivtag 'primitive) arity id #t))

(define (prim-reverse binding)
  (if (primitive? binding)
    (reverse (cons (not (primitive-reverse? binding))
                   (cdr (reverse binding))))
    (raise "type error")))

; primitives
(define primitive-arity cadr)
(define primitive-id caddr)
(define primitive-reverse? cadddr)
(define (primitive? obj)
  (and (list? obj) (equal? (car obj) (mklvivtag 'primitive)) (= (length obj) 4)))

; primitive call
(define (stPrimCall state binding)
  (let* ((rfunc (if (primitive-reverse? binding) values reverse))
         (fnNArgs (delay (primitive-arity binding)))
         (fnArgs (delay (stStackNPop state (force fnNArgs))))
         (fnCompResult
           (lambda ()
             (eRight (apply (eval (primitive-id binding))
                            (rfunc (fromLeftRight (force fnArgs)))))))
         (fnResult (delay (with-exception-catcher 
                            (exceptionHandler #f)
                            fnCompResult))))
    (cond ((eLeft? (force fnArgs)) (force fnArgs))
            ; if there aren't enough args, the procedure fails
            ; and the stack doesn't get rewound any further
            ; note that if stStackNPop fails, it will rewind what
            ; it did
          ((eLeft? (force fnResult))
           (rewind state
                   (reverse (fromLeftRight (force fnArgs)))
                   (fromLeftRight (force fnResult))))
            ; if the primitive application fails, put the args
            ; back on the stack
          (else (stStackPush state (fromLeftRight (force fnResult)))))))
            ; else push the new value onto the stack

; lviv-eval evaluates concrete syntax; this function evaluates the AST
; the AST and the concrete syntax are _almost_ the same, except that
; variable references get resolved and lists get applied as syntax
(define (stEval state)
  (let* ((fnEArg (stStackPop state))
         (fnArg (fromLeftRight fnEArg))
         (lookupPush
           (lambda (env name)
             (let ((lkRef (envLookupBinding env name)))
               (if (eLeft? lkRef)
                 (eLeft "lookup failed")
                 (stStackPush state (fromLeftRight lkRef)))))))
    (cond ((eLeft? fnEArg) fnEArg)                                                  ; pop failed
          ((static-symbol-elm? fnArg) (lookupPush (caddr fnArg) (cadr fnArg)))      ; lookup static
          ((auto-symbol-elm? fnArg) (lookupPush (stGetEnv state) (cadr fnArg)))     ; lookup auto
          ((quote-symbol-elm? fnArg)
           (stStackPush state (lviv-eval state fnArg)))                             ; handle like the literal was just typed in
          ((posn-symbol-elm? fnArg) (rewind state (list fnArg) "not implemented"))  ; haven't done positionals yet
          ((lviv-tagged? fnArg) (stStackPush state fnArg))                          ; idempotent through eval
          ((list? fnArg) (map (lambda (item) (lviv-apply state item)) fnArg))       ; apply as if typed in
          (else (stStackPush state (lviv-eval state fnArg))))))                     ; else assume it's idempotent

; since lviv-apply always works on AST, stApply and lviv-apply are
; basically identical, but we have to wrap a safe pop around the
; former to get the latter
(define (stApply state) ; 
  (let* ((fnEArg (stStackPop state))            ; pop off element
         (fnArg (fromLeftRight fnEArg)))
    (cond ((eLeft? fnEArg) fnEArg)              ; pop unsuccessful?
          ((thunkElm? fnArg)                    ; thunk?
           (begin (stStackPush state (thunkElm->elm fnArg)) ; unwrap
                  (stEval state)))                          ; eval
          (else (lviv-apply state fnArg)))))    ; otherwise, just apply it like anything else

; ###############
; #### EVAL #####
; ###############

(define (x-symbol? char)
  (lambda (symb)
    (and (symbol? symb) 
         (let ((symb-str (symbol->string symb)))
           (and (> (string-length symb-str) 1) ; '& is not a legal static symbol
                (eq? char (string-ref symb-str 0)))))))

(define (x-symbol->symbol test err)
  (lambda (symb)
    (if (not (test symb))
      (raise err)
      (let ((symb-str (symbol->string symb)))
        (string->symbol (substring symb-str 1 (string-length symb-str)))))))

(define (x-symbol-elm? sym len)
  (lambda (elm)
    (and (pair? elm)
         (equal? (mklvivtag sym) (car elm))
         (= len (length elm)))))

(define static-symbol? (x-symbol? #\&))
(define static-symbol->symbol (x-symbol->symbol static-symbol? "invalid static symbol"))
(define (mkStaticSymbolElm symb env)
  (list (mklvivtag '&) symb env))
(define static-symbol-elm? (x-symbol-elm? '& 3))
(define static-symbol-env caddr)
(define static/auto-symbol-sym cadr)

(define posn-symbol? (x-symbol? #\!))
(define posn-symbol->symbol (x-symbol->symbol posn-symbol? "invalid position symbol"))
(define (mkPosnRefElm n)
  (list (mklvivtag '!) n))
(define posn-symbol-elm? (x-symbol-elm? '! 2))

(define auto-symbol? (x-symbol? #\@))
(define auto-symbol->symbol (x-symbol->symbol auto-symbol? "invalid auto symbol"))
(define (mkAutoSymbolElm symb)
  (list (mklvivtag '@) symb))
(define auto-symbol-elm? (x-symbol-elm? '@ 2))

(define quote-symbol? (x-symbol? #\*))
(define quote-symbol->symbol (x-symbol->symbol quote-symbol? "invalid quote symbol"))
(define quote-symbol-elm? symbol?)

(define reverse-symbol? (x-symbol? #\:))
(define reverse-symbol->symbol (x-symbol->symbol reverse-symbol? "invalid reverse symbol"))

(define (symbol-elm? item) (or (static-symbol-elm? item)
                               (posn-symbol-elm? item)
                               (auto-symbol-elm? item)
                               (quote-symbol-elm? item)))

(define (mkStackOpElm op) (cons (mklvivtag 'stackop) op))
(define (stackOpElm? op)
  (and (pair? op) (equal? (mklvivtag 'stackop) (car op)) (procedure? (cdr op))))
(define stackOpElm->stackop cdr)

(define (mkThunkElm op) (cons (mklvivtag 'thunk) op))
(define (thunkElm? op)
  (and (pair? op) (equal? (mklvivtag 'thunk) (car op))))
(define thunkElm->elm cdr)

(define (lviv-eval state item)
  (cond ((eq? item 'nop) item) ; nop is same in the AST
        ((eq? item 'env) item) ; env is same in the AST
        ((eq? item 'if) item)
        ((static-symbol? item) ; &foo -> (& foo env)
         (mkStaticSymbolElm (static-symbol->symbol item)
                            (stGetEnv state)))
        ((auto-symbol? item)   ; @foo -> (@ foo)
         (mkAutoSymbolElm (auto-symbol->symbol item)))
        ((posn-symbol? item)   ; !1   -> (! 1)
         (mkPosnRefElm (posn-symbol->symbol item)))
        ((quote-symbol? item)  ; *bar -> bar
         (quote-symbol->symbol item))
        ((stStackOp? item) (mkStackOpElm (stStackOp? item))) ; primitive stack op
        ((reverse-symbol? item) ; :cons -> cons in reverse
         (let* ((iLBind (stEnvLookupBinding state (reverse-symbol->symbol item)))
                (iBind (fromLeftRight iLBind)))
           (cond ((eLeft? iLBind) iLBind)
                 ((primitive? iBind) (prim-reverse iBind))
                 (else (eLeft "cannot reverse non-op")))))
        ((symbol? item)        ; look up symbol in present env
         (let ((iLBind (stEnvLookupBinding state item)))
           (if (eLeft? iLBind) ; did lookup succeed?
             (mkAutoSymbolElm item) ; no---make auto symbol
             (fromLeftRight iLBind)))) ; yes---pass it on
        ((list? item)
         (map (lambda (x) (lviv-eval state x)) item))
        ((pair? item)
         (cons (lviv-eval state (car item))
               (lviv-eval state (cdr item))))
        (else item)))

(define (lviv-apply state item)
  ((lambda (result) (if (eLeft? result) (stackError result) result))
  (cond ((eq? item 'nop) (eRight '())) ; nop does nothing
        ((eq? item 'env) ; env will show the present environment
         (eRight (pp (stGetEnv state)) (newline)))
        ((eLeft? item) item)
        ((eq? item 'if)
         (begin (stStackSwapUnless myState)
                (stStackDrop myState)
                (stEval myState)))
        ((symbol-elm? item)
         (stStackPush state item))
        ((stackOpElm? item) ((stackOpElm->stackop item) state))
        ((primitive? item) (stPrimCall state item))
        (else (stStackPush state item))))) ; else just push it on the stack

(define (lviv-repl state input)
  (if (eq? input '#!eof)
    #f
    (begin 
      (if (string? input)
        (with-exception-catcher (exceptionHandler #t) (lambda ()
        (map (lambda (x) (lviv-apply state (lviv-eval state x)))
             (call-with-input-string input read-all)))))
      (lviv-ppstack (stGetStack state) (stEnvLookupBinding state '_stack_display_depth))
      (display "> ")
      (lviv-repl state (read-line)))))

(define _stack_display_depth 10)

(define (lviv-ppstack stack eDepth)
  (let ((depth (if (and (eRight? eDepth) (exact? (fromLeftRight eDepth)))
                 (fromLeftRight eDepth)
                 _stack_display_depth)))
    (map pp (reverse (take depth stack)))))

; #################
; #### PRELUDE ####
; #################

(define myState (mkEmptyState))

(stEnvUpdateBinding myState (cons '+ (mkPrimBinding '+ 2)))
(stEnvUpdateBinding myState (cons '* (mkPrimBinding '* 2)))
(stEnvUpdateBinding myState (cons '- (mkPrimBinding '- 2)))
(stEnvUpdateBinding myState (cons '/ (mkPrimBinding '/ 2)))
(stEnvUpdateBinding myState (cons 'cons (mkPrimBinding 'cons 2)))
(stEnvUpdateBinding myState (cons 'rcons (mkRPrimBinding 'cons 2)))
(stEnvUpdateBinding myState (cons 'eq? (mkPrimBinding 'eq? 2)))

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


; ###############
; #### TESTS ####
; ###############

(define (test x msg)
  (or x (raise msg)))

(define (testLookup x val)
    (test (and (eRight? (stEnvLookupBinding myState x))
               (equal? (fromLeftRight (stEnvLookupBinding myState x)) val))
          (string-append (symbol->string x) " lookup failed")))

(define (testStack val msg)
  (test (equal? (stGetStack myState) val) msg))

(stStackPush myState 1)
(stStackPush myState 2)
(stStackPush myState 3)
(stStackPush myState 4)
(stStackPush myState 5)
(stStackPush myState 2)
(testStack '(2 5 4 3 2 1) "push tests failed")

(stStackSwap myState)
(testStack '(5 2 4 3 2 1) "swap test failed")

(stStackDrop myState)
(testStack '(2 4 3 2 1) "drop test failed")

(stStackDropN myState)
(testStack '(2 1) "dropN test failed")

(stStackClear myState)
(testStack '() "clear test failed")

(stStackPush myState 1)
(stStackPush myState 2)
(stStackPush myState 3)
(stStackPush myState 4)
(stStackPush myState 5)
(stStackRollN myState)
(testStack '(5 4 3 2 1) "opN not enough args test failed")
(stStackPush myState 3)
(stStackRollN myState)
(testStack '(3 5 4 2 1) "rollN test failed")

(stStackPush myState 2)
(stStackUnrollN myState)
(testStack '(5 3 4 2 1) "unrollN test failed")

(stStackDup myState)
(testStack '(5 5 3 4 2 1) "dup test failed")

(stStackPush myState 3)
(stStackDupN myState)
(testStack '(5 5 3 5 5 3 4 2 1) "dupN test failed")

(stStackPush myState 'a)
(stStackDupN myState)
(testStack '(a 5 5 3 5 5 3 4 2 1) "opN non-numeric test failed")

(stStackOver myState)
(testStack '(5 a 5 5 3 5 5 3 4 2 1) "over test failed")

(stStackPush myState '5)
(stStackPickN myState)
(testStack '(3 5 a 5 5 3 5 5 3 4 2 1) "pickN test failed")

(stPrimCall myState (mkPrimBinding '+ 2))
(testStack '(8 a 5 5 3 5 5 3 4 2 1) "hidden primitive test failed")

(stStackDropN myState)
(testStack '(2 1) "dropN test failed")

(stStackPush myState '#t)
(stStackDropUnless myState)
(testStack '(2 1) "#t dropUnless test failed")

(stStackPush myState '#f)
(stStackDropUnless myState)
(testStack '(1) "#f dropUnless test failed")

(stStackPush myState 5)
(stStackDropUnless myState)
(testStack '(5 1) "non-bool dropUnless test failed")

(stStackPush myState #f)
(stStackDropIf myState)
(testStack '(5 1) "#f dropIf test failed")

(stStackPush myState #t)
(stStackDropIf myState)
(testStack '(1) "#t dropIf test failed")

(stStackDropIf myState)
(testStack '(1) "non-bool dropIf test failed")

(stStackPush myState 5)
(stStackPush myState #t)
(stStackSwapIf myState)
(testStack '(1 5) "#t swapIf test failed")

(stStackPush myState 5)
(stStackPush myState #f)
(stStackSwapIf myState)
(testStack '(5 1 5) "#f swapIf test failed")

(stStackSwapIf myState)
(testStack '(5 1 5) "non-bool swapIf test failed")

(stStackPush myState #f)
(stStackPush myState #t)
(stStackSwapUnless myState)
(testStack '(#f 5 1 5) "#t swapUnless test failed")

(stStackSwapUnless myState)
(testStack '(1 5 5) "#f swapUnless test failed")

(stStackSwapUnless myState)
(testStack '(1 5 5) "non-bool swapUnless test failed")

(stEnvUpdateBinding myState '(a . 1))
(stEnvUpdateBinding myState '(b . 2))
(stEnvUpdateBinding myState '(c . 3))
(stEnvUpdateBinding myState '(d . 4))
(stEnvUpdateBinding myState '(e . 5))
(stEnvUpdateBinding myState '(a . 6))
(stEnvDelBinding myState 'd)

(test (eLeft? (stEnvLookupBinding myState 'd)) "d still bound!?")
(testLookup 'e 5)

(stEnvNewChild myState)
(stEnvUpdateBinding myState '(f 100))
(stEnvUpdateBinding myState '(b 200))

(testLookup 'e 5)
(testLookup 'f '(100))
(testLookup 'b '(200))
(test (eLeft? (stEnvLookupBinding myState 'd)) "d still bound!? (2)")

(display (stEnvLookupBinding myState 'c)) (newline)
(display (stEnvLookupBinding myState 'd)) (newline)
(display (stEnvLookupBinding myState 'b)) (newline)

(display myState) (newline)
(stEnvParent myState)
(stEnvParent myState)
(stEnvParent myState)
(stEnvParent myState)
(display myState) (newline)

(test (stGlobalEnv? myState) "should be global env here")

(stEnvNewChild myState)
(stEnvUpdateBinding myState '(f 100))
(stEnvUpdateBinding myState '(b 200))

(test (not (stGlobalEnv? myState)) "should not be global env here")

(stEnvParent myState)
(stEnvParent myState)
(test (stGlobalEnv? myState) "should be global env here")

(lviv-apply myState (lviv-eval myState '+))

(test (equal? (stGetStack myState) '(6 5)) "state of stack is wrong after +")

(lviv-apply myState (lviv-eval myState '@b))
(lviv-apply myState (lviv-eval myState '@a))
(display myState) (newline)

(test (eLeft? 
        (with-exception-catcher (exceptionHandler #f)
         (lambda () (stPrimCall myState (lviv-eval myState '+)))))
        "call to + failed to fail")
(test (equal? (stGetStack myState) (list (mkAutoSymbolElm 'a) (mkAutoSymbolElm 'b) 6 5))
      "stack is in wrong state after type failure")
(testLookup 'cdr (mkPrimBinding 'cdr 1))

(display myState) (newline)

(lviv-repl myState #f)

