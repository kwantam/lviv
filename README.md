# lviv

`lviv` is a functional RPN programming language.

The name comes from the city of Lviv, where Jan Lukasiewicz was born. Lukasiewicz invented prefix or "Polish" notation to simplify the syntax of sentential logic; later, Burks, Warren, and Wright (and later Bauer and Dijkstra) proposed postfix, or "reverse Polish" notation as a good fit for stack based machines.

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

## Stack operations

The contents of the stack often represent most or all of the program's state. Thus, primitive stack operations underly most higher level operations in `lviv`.

There are no explicit "push" and "pop" operations; values are pushed as they're entered, and popped as needed for function application.

### `swap`

Swap the 0th and 1st entry in the stack.

    > 1 2
    1
    2
    > swap
    2
    1

### `drop`
### `<n> dropN`
### `clear`

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

### `<n> roll`
### `<n> unroll`

`roll` and `unroll` pop the 0th element off the stack and perform a circular shift upwards (`roll`) or downwards (`unroll`) involving `n` elements.

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

### `dup`
### `<n> dupN`

`dup` pushes a copy of the 0th element onto the stack. `dupN` pops the 0th argument off the stack, and pushes copies of the top `n` elements onto the stack.

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

### `<n> pick`
### `over`

`pick` pops the 0th element off the stack, then pushes a copy of the `n`th remaining element onto the stack. `over` is equivalent to `1 pick`.

    > 1 2 3
    1
    2
    3
    > 2 pick
    1
    2
    3
    1

Note that the `pick` implementation on HP calculators uses one-based rather than 0-based indexing.

### `depth`

Pushes the depth of the stack prior to the `depth` operation onto the stack.

    > 0
    0
    > depth
    0
    1
    > 2 dropN depth
    0

### `<bool> swapIf`
### `<bool> swapUnless`
### `<bool> dropIf`
### `<bool> dropUnless`

`swapIf` pops the 0th element off the stack, and then performs a `swap` if that element evaluated to a true value. `swapUnless` does the same for a false value.

    > 1 2 True swapIf drop
    2
    > clear
    > 1 2 False swapUnless -
    1

### `nop`

`nop` does nothing. It is useful for operations that conditionally modify the stack.

### Mathematical operations

Many mathematical operations, including arithmetic, trigonometric, and complex functions, are available.

## Functional syntax

`lviv` supports a full range of standard functional syntax.

### Lists

Lists in `lviv` have a syntax similar to those in Haskell. Formally, a list is defined either as the empty list, or as the result of the cons operation `:` on an element and a list. `head` and `tail` produce the element and the trailing list, respectively. `uncons` pops a list off the stack and pushes on the tail followed by the head. `++_` and `_++` are the left and right append operators, respectively. (The underscore indicates which side the 0th element of the stack goes.)

`[a,[b,c],d]`-like syntax can be used to create a list directly.

    > []
    []
    > 1 :
    [1]
    > 2 :
    [1,2]
    > [3,4] ++_
    [1,2,3,4]
    > uncons
    [2,3,4]
    1
    > :
    [1,2,3,4]
    > tail
    [2,3,4]
    > head
    2

Strings are just lists of characters, so they can be operated upon by all list operations.

### Tuples

Tuples in `lviv` are also similar to their Haskell counterparts. The tuple operators `,`, `,,`, et cetera pop a number of variables from the stack and push on a newly created tuple. `(a,(b,c),d)` style syntax can also be used.

`fst` and `snd` are defined in the prelude, but in general tuples should be deconstructed via pattern matching.

    > b a ,
    (a,b)
    > b a ,,
    (a,b,(a,b))
    > (c,d,(e,f))
    (a,b,(a,b))
    (c,d,(e,f))

### Environment bindings

#### `<val> <identifier> define`

`define` binds the identifier in the 0th position on the stack with the value in the 1st in the containing environment.

Identifiers can contain alphanumerics or any of `! $ % + - < = > ? @ ^ _ ~`, but must begin with an alphabetic character.

When a bound variable is placed on the stack, it is immediately replaced by its value. To invoke the identifier and force delayed binding, either the `&` or `*` prefix can be used. The `&` prefix produces a binding to the present environment (static scope), whereas the `*` prefix binds to the enclosing environment at the time of evaluation (dynamic scope).

    > 1 z define
    --> z : 1
    > 2 z
    2
    1
    > -
    1
    > &z +
    #<thunk { 1 &z + }>
    > 2 z define
    --> z : 2
    #<thunk { 1 &z + }>
    > eval
    3

### Thunks

`lviv` represents delayed computations using thunks. The `eval` function attempts to apply a thunk, popping positionally referenced variables from the stack and binding them before evaluation. If the thunk references unbound variables, an error results from the computation and the stack is unmodified. `eval`ing a literal expression has no effect.

    > 1 eval
    1

#### Braced thunks

Thunks can be denoted by enclosing an expression in braces (`{}`).

    > { 1 0 / }
    #<thunk { 1 0 / }>
    > eval
    --> error: division by zero
    #<thunk { 1 0 / }>
    > drop 1 z define { 1 z + }
    #<thunk { 1 1 + }>
    > 2 z define eval
    2

#### Unbound identifiers

Referencing an unbound identifier in an expression results in a thunk where the unbound identifier is dynamically scoped (as if invoked with `*identifier`; you will soon see that this is useful because of how `lambda` works). Lexically scoped unbound identifiers can also be invoked using `&`.

    > x
    *x : #<unbound>
    > 1 +
    #<thunk { *x 1 + }>
    > 3 * &y -
    #<thunk { *x 1 + 3 * &y -}>
    > eval
    --> error: unbound variables x,y in eval
    #<thunk { *x 1 + 3 * &y - }>
    > &z /
    #<thunk { *x 1 + 3 * &y - &z / }>
    > 1 z define eval
    --> error: unbound variables x,y in eval
    #<thunk { *x 1 + 3 * &y - &z / }>

Note that when an error occurs, no bindings take place. If z were redefined in the lexical scope before x and y became available, the new value of z would apply when the expression was applied.

#### Positional identifiers

Positional identifiers are identifiers of the form `#[0-9]+` which are unbound until evaluated. In a thunk, these identifiers represent the corresponding stack positions at the time the thunk is evaluated.

    > 1 #0
    1
    #0 : #<unbound>
    > eval
    1
    > #0 1 +
    1
    #<thunk { #0 1 +}>
    > 2 swap
    1
    2
    #<thunk { #0 1 +}>
    > eval
    1
    3

### Lambdas

`lambda` combines a thunk and a positional binding list into a function. Thunks using positional identifiers can be used in lambdas, but it's probably better not to for clarity's sake.

Positional binding lists map variables inside the thunk to positional references on the stack. List elements are numbered from left to right starting at 0. Any dynamically scoped variables in the thunk that correspond to identifiers in the positional binding list become lexically scoped to the lambda, resulting in the lambda's closing over the variables in the positional binding list.

    > x
    *x : #<unbound>
    > 1 +
    #<thunk { *x 1 + }>
    > *y *
    #<thunk { *x 1 + *y * }>
    > [y,x] lambda
    #<lambda [y,x] thunk { &x 1 + &y * }>
    > 2 1
    #<lambda [y,x] thunk { &x 1 + &y * }>
    2
    1
    > 3 roll
    2
    1
    #<lambda [y,x] thunk { &x 1 + &y * }>
    > eval
    3

The above lambda is equivalent to

    > #1 1 + #0 *
    #<thunk { #1 1 + #0 * }>
    > 2 1 3 roll
    2
    1
    #<thunk { #1 1 + #0 * }>
    > eval
    3

### `let`

`let` is similar to `lambda`: it takes a thunk and a binding list. `let` is evaluated immediately and the result of the evaluation is pushed onto the stack. If the `let` expression contains unbound variables after evaluation, its result is a thunk.

The `let` expression uses a named binding list rather than a positional binding list. A named binding list is a list of tuples which are bound sequentially and then evaluated. In this way, `let` behaves much like Scheme's `letrec`: variables inside the named binding list may reference other variables in the list whether they are bound before or after. Like the `lambda`, dynamically scoped variables in the thunk and the RHS of the named binding tuples that correspond to identifiers in the named binding list become lexically scoped to the `let`. This means that named bindings need not shadow variables in enclosing environments.

    > 2 a define
    --> a : 2
    > { &a *a b + * } [(a,1),(b,a &a +)] let
    8
    > z +
    #<thunk { 8 *z + }>
    > a * [(a,1)] let
    #<thunk { 8 *z + 1 * }>

### `<consequent> <alternative> <test> if`

`if` is actually just equivalent to `swapUnless drop eval`. To be sure that only one of the consequent or alternative is evaluated, make sure to delay their evaluation!

    > { 1 } { 2 } #t if
    1
    > { 1 } { 2 } #f if
    1
    2
    > 1 2 #f swapUnless drop eval
    1
    2
    2

Here's a contrived example where you'd need to delay evaluation:
    
    > { 1 0 / } { NaN } 0 0 /= if
    NaN

If you didn't delay evaluation, you'd end up evaluating `1 0 /` and immediately generate an error.

## Other operations

### exception handling

tbd

### `tstk`, `untstk`, and `rtstk`
###### probably YAGNI

`tstk` moves aside the present stack and replaces it with an empty temporary stack. `untstk` removes the temporary stack and restores the previous one. `rtstk` pops the 0th value off the temporary stack, restores the previous stack, and pushes this value.

`tstk` calls can be nested; each `untstk` or `rtstk` ascends one level of nesting.

### namespacing
###### probably YAGNI

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

### named stacks
###### probably YAGNI

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


