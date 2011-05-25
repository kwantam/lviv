# lviv

lviv is a functional RPN programming language.

The name comes from the city of Lviv, where Jan Lukasiewicz was born. Lukasiewicz invented prefix or "Polish" notation to simplify the syntax of sentential logic; later, Burks, Warren, and Wright (and even later Bauer and Dijkstra) proposed postfix, or "reverse Polish" notation as a good fit for stack based machines.

## RPN basics

If you've used an HP calculator, you're probably familiar with how RPN works. Expressions are entered by pushing entries onto a stack and applying operators to the stack. Operators pop a defined number of operands off the stack, perform a computation, and push the result back onto the stack. For example,

    > 1
    1
    > 2
    1
    2
    > +
    3
    > 2 *
    6
    > 6 * sqrt
    6

### Order of application

By convention, RPN operations are applied in conventional order: prefix and postfix notation are related by simply translating the operator from the beginning to the end of the expression. `- 6 1` becomes `6 1 -`, and both should equal 5. This seems somewhat logical for a calculator, since it matches our intuition for the basic noncommutative arithmetic operations.

Unfortunately, this order seems (to me) pretty clunky when recursively applying operations that consume and return multiple values. In this context, it seems more sensible to think of function application as repeatedly applying stack values to a curried function until it returns a value. (lviv functions are not automatically curried like Haskell functions, so perhaps one could succesfully argue that I only think this way because I write too much Haskell.)

By convention, all operations in lviv use reflected-prefix application order. However, functions can be applied in reverse by prepending them with `:`. Thus, `6 1 -` yields `-5`, but `6 1 :-` gives `5` as expected by someone used to an RPN calculator.

## Stack operations

The contents of the stack often represent most or all of the program's state. Thus, primitive stack operations underly most higher level operations in lviv.

There are no explicit "push" and "pop" operations; values are pushed as they're entered, and popped as needed for function application.

### `swap`

Swap the 0th and 1st entry in the stack.

    > 1 2
    1
    2
    > swap
    2
    1

### `drop`, `<n> dropN`, `clear`

`drop` removes the 0th element from the stack. `dropN` pops the 0th entry off the stack, and then drops that number of remaining entries. `clear` drops all entries from the stack.

    > 1
    1
    > drop
    > 1 3 2
    1
    3
    2
    > dropN
    > 

### `<n> roll`, `<n> unroll`

`roll` and `unroll` pop the 0th element off the stack and perform a circular shift upwards (`roll`) or downwards (`unroll`) involving *n* elements.

    > 1 2 3 4 5 6
    1
    2
    3
    4
    5
    6
    > 3 roll
    1
    2
    3
    5
    6
    4
    > unroll
    1
    6
    2
    3
    5

### `dup` `<n> dupN`

`dup` pushes a copy of the 0th element onto the stack. `dupN` pops the 0th argument off the stack, and pushes copies of the top *n* elements onto the stack.

    > 1 2 3 dup
    1
    2
    3
    3
    > 3 dupN
    1
    2
    3
    3
    2
    3
    3

### `over`, `<n> pick`

`pick` pops the 0th element off the stack, then pushes a copy of the *nth* element onto the stack. `over` is equivalent to `2 pick`.

    > 1 2 3
    1
    2
    3
    > over
    1
    2
    3
    2
    > 2 pick
    1
    2
    3
    2
    3

### `depth`

Pushes the depth of the stack prior to the `depth` operation onto the stack.

    > 0
    0
    > depth
    0
    1
    > 2 dropN depth
    0

### `<bool> swapIf`, `<bool> swapUnless`, `<bool> dropIf`, `<bool> dropUnless`

`swapIf` pops the 0th element off the stack, and then performs a `swap` if that element was `#t`. `swapUnless` does the same for `#f`. `dropIf` and `dropUnless` behave similarly. Note that these operations only accept the boolean values `#t` or `#f`; other values result in a type error.

    > 1 2 #t swapIf drop
    2
    > clear
    > 1 2 #f swapUnless -
    -1

### `nop`

`nop` does nothing. It is useful for operations that conditionally modify the stack.

### `env`

`env` shows the current environment. It can be useful for debugging.

### Mathematical operations

Many mathematical operations, including arithmetic, trigonometric, and complex functions, are available.

## Functional syntax

lviv supports a full range of standard functional syntax.

### Lists

Lists in lviv should be familiar to LISP users. Formally, a list is defined either as the empty list (`nil` or `()`), or as the result of the `cons` operation on an element and a list. An element is anything that can be a stack entry.

`car` and `cdr` produce the element and the trailing list, respectively. `uncons` pops a list off the stack and pushes on the cdr, then the car.

`(a (b c) d)`-like syntax can be used to create a list directly.

    > nil
    ()
    > 1 cons
    (1)
    > 2 cons
    (2 1)
    > (3 4) :append
    (2 1 3 4)
    > uncons
    (1 3 4)
    2
    > cons
    (2 1 3 4)
    > cdr
    (1 3 4)
    > car
    1

### Tuples

Tuples in lviv can also be constructed using `cons`. A tuple is simply an unterminated list.

    > b a cons
    (a . b)
    > b a cons cons
    ((a . b) a . b)
    > (c d . (e . f))
    ((a . b) a . b)
    (c d e . f)

### Environment bindings

#### `<val> <identifier> define`

`define` binds the identifier in the 0th position on the stack with the value in the 1st in the containing environment. If the identifier is a static variable, the binding is placed in the attached environment. Otherwise, the binding is placed in the current environment.

Identifiers can contain alphanumerics or any of `! $ % & * + - . / : < = > ? @ ^ _ ~`, but must begin with a character that cannot begin a number and is not otherwise reserved (i.e., any valid character other than `. + - & * !`).

When a bound variable is placed on the stack, it is immediately replaced by its value. To invoke the identifier and force delayed binding, the `&` or `*` sigil can be used. The `&` sigil indicates that the variable is statically bound in enclosing environment, whereas the `*` prefix simply places the identifier on the stack, leaving its binding to an environment until evaluation.

    > 1 *z define
    > 2 z
    2
    1
    > :-
    1
    > (&z +) :cons thunk
    #<thunk ( 1 &z + )>
    > 2 z define
    #<thunk ( 1 &z + )>
    > apply
    3

#### `<identifier> undef`, `<identifier> undefLocal`

`undef` is used to delete a binding from the environment. If the identifier is a static variable, the binding is searched starting in the attached environment and removed if found. Otherwise, the search begins in the current environment.

Note that `undef` will search from the present environment level all the way up to the top. To prevent the search from extending into the parent of the starting search environment, use `undefLocal` instead.

#### Scope

Static variables (invoked with `&`) bind immediately to the enclosing environment when the variable is put on the stack. The static binding element carries a reference to its environment with it, soit can be properly dereferenced even when enclosed in another environment (e.g., a let or lambda) where its name is shadowed.

Unbound identifiers (invoked with `*`) have no binding when they are put on the stack. Instead, they are bound when they are evaluated. Since lambdas and lets carry an environment with them, unbound identifiers are still statically scoped inside these elements. However, an unbound identifier inside a thunk takes its value from the enclosing environment at the time of application (as is the case with the `delay`-`force` relationship in Scheme).

### `eval` and `apply`

`eval` evaluates the top entry on the stack. Variables are dereferenced, but most other expressions are idempotent. This is probably nonintutive, so a bit of explanation is in order.

In LISP, evaluating a list results in a computation as if that list were typed in as code. In lviv, lists do not represent a fundamental unit of computation, so evaluating a list merely dereferences the enclosed variables. Similarly, application in LISP combines a list of arguments with a function. In lviv, all application involves an element interacting with the stack. If the element is a function, its application involves popping arguments and pushing results. Otherwise, the application of an element to the stack simply results in that element being pushed onto the stack.

To cause its contents to be computed as if entered at the prompt, a list must be turned into a thunk and then applied using `apply`. Alternatively, it can be turned into a lambda with an empty argument list and then applied. The fundamental difference between a lambda and a thunk is that the latter is bound to an environment when applied, whereas the former is bound to an environment when created.

    > 1 eval
    1
    > *a define
    > *a
    a
    > eval
    > (*a 2) :cons
    (1 a 2)
    > 4 *a define
    (1 a 2)
    > eval
    (1 4 2)

Other than when working on thunks, `apply` takes the top element off the list and applies it as it just typed into the REPL. Thus, the semantics of `apply` are not exactly the same as in LISP: in lviv, `apply` applies the top element on the stack to the stack. Most elements are idempotent through such application (i.e., applying 1 to the stack just puts 1 on the stack); lambdas and primitives result in computation when applied to the stack.

    ...continued from above...
    (1 4 2)
    > 1 apply
    (1 4 2)
    1
    > *cons
    (1 4 2)
    1
    cons
    > apply
    (1 4 2)
    1
    cons
    > eval
    (1 4 2)
    1
    #<primitive cons>
    > apply
    (1 1 4 2)

thunks are a special case for `apply`; see below.

### Thunks

lviv represents explicitly delayed computations using thunks. The `apply` function unwraps a thunk and evaluates it as if its contents were typed into the REPL. Thunks are idempotent through `eval`, which means that their bindings are delayed until they are applied. This means that `thunk`s can introduce dynamic scoping: if a thunk is stored in a variable, it can be retrieved by two different functions. When applied, its scope is determined by the function it is called in, not by its definition scope.

    > (1 *z +) thunk
    #<thunk ( 1 z + )>
    > eval
    #<thunk ( 1 z + )>
    > dup 2 *z define apply
    #<thunk ( 1 z + )>
    3
    > 15 + *z define apply
    19

### Positional identifiers *TODO*

Positional identifiers are identifiers of the form `![0-9]+` which are unbound until evaluated. In a thunk, these identifiers represent the corresponding stack positions at the time the thunk is evaluated.

    > 1 !0
    1
    !0
    > eval
    1
    > (!0 1 +) thunk
    1
    #<thunk ( !0 1 + )>
    > 2 swap
    1
    2
    #<thunk ( !0 1 + )>
    > eval
    1
    3

### `<arity> <identifier> primitive`

`primitive` is used to bind an underlying scheme operation into a lviv element. For example,

    > 2 *expt primitive
    #<primitive expt>
    > 2 3 3 roll
    2
    3
    #<primitive expt>
    > apply
    9
    > 2 *expt primitive *expt define 2 :expt
    81

### Lambdas

`lambda` combines a delayed computation and a binding list into a function. Positional identifiers cannot be used with a `lambda`.

Binding lists map variables inside the thunk to positions on the stack at application time. List elements are numbered from left to right starting at 0.

    > *x
    x
    > (1 +) :cons
    ( x 1 #<primitive +> )
    > (*y *) :append
    ( x 1 #<primitive +> y #<primitive *> )
    > (*y *x) lambda *xyfunc define
    > 2 1
    2
    1
    > xyfunc
    3
    > 2 *xyfunc eval
    3
    2
    #<lambda ( x 1 #<primitive +> y #<primitive *> ) ( y x )>
    > apply
    8

The above lambda is equivalent to

    > (swap 1 + swap *) thunk
    #<thunk ( #<stackOp swap> 1 #<primitive +> #<stackOp swap> #<primitive *> )>
    > 2 1 3 roll
    2
    1
    #<thunk ( #<stackOp swap> 1 #<primitive +> #<stackOp swap> #<primitive *> )>
    > apply
    3

### `let` *TODO*

`let` is similar to `lambda`: it combines a delayed computation and a binding list. `let` is evaluated immediately and the result of the evaluation is pushed onto the stack.

    > 2 a define
    > ( &a a b + * ) ( ( a . 1 ) ( b . ( a &a + ) ) ) let
    8
    > 6 *z define
    8
    > (*z +) :cons
    ( 8 z + )
    > (*a *) :append ( (a . 1) ) let
    14

### `<consequent> <alternative> <test> if`, `<consequent> <alternative> <test> unless`

`if` is actually just a short way of saying `swapUnless drop thunk apply`.

    > 1 (nop) (0 /) #t if
    1
    > (3 -) (3 +) #f if
    4
    > (3 :-) (3 +) #t swapUnless drop thunk apply
    1
    > ((2) (1) #t if) (0) #t if
    2

The first example above illustrates that only one of the consequent or alternative is applied as one would expect.

`unless` is equivalent to `swapIf drop thunk apply`.

### `cond` *TODO*

## Other operations

### exception handling *TODO*


### `tstk`, `untstk`, and `rtstk` *TODO*

`tstk` moves aside the present stack and replaces it with an empty temporary stack. `untstk` removes the temporary stack and restores the previous one. `rtstk` pops the 0th value off the temporary stack, restores the previous stack, and pushes this value.

`tstk` calls can be nested; each `untstk` or `rtstk` ascends one level of nesting.

### namespacing *TODO*

    > :: namespace
    --> namespace : ::
    > ::foo namespace
    --> namespace : ::foo
    > bar namespace
    --> namespace : ::foo::bar
    > 1 z private define
    --> ::foo::bar::z : 1 (private)
    > 2 y define
    --> ::foo::bar::y : 2
    > :: namespace
    --> namespace : ::
    > ::foo::bar::z
    --> error: attempted to access private variable ::foo::bar::z
    > ::foo::bar::y
    2

### named stacks *TODO*

    > __ stack
    --> stack : __
    > 1 2 3
    1
    2
    3
    > __foo stack
    --> stack : __foo
    > 1
    1
    > __ stack
    --> stack : __
    1
    2
    3
