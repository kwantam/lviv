# lviv

`lviv` is a functional RPN programming language.

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

Lists in `lviv` have a syntax similar to those in Haskell, but are heterogeneous collections more akin to LISP lists. Formally, a list is defined either as the empty list, or as the result of the cons operation `:` on an element and a list. An element is anything that can be a stack entry.

`head` and `tail` produce the element and the trailing list, respectively. `uncons` pops a list off the stack and pushes on the tail and the head. `++_` and `_++` are the left and right append operators, respectively. (The underscore indicates which side the 0th element of the stack goes.)

`[a,[b,c],d]`-like syntax can be used to create a list directly.

    > []
    []
    > 1 :
    [1]
    > 2 :
    [2,1]
    > [3,4] ++_
    [2,1,3,4]
    > uncons
    [1,3,4]
    2
    > :
    [2,1,3,4]
    > tail
    [1,3,4]
    > head
    1

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

Identifiers can contain alphanumerics or any of `! $ % & * + - . / : < = > ? @ ^ _ ~`, but must begin with a character that cannot begin a number and is not otherwise reserved (i.e., any valid character other than `. + - & @`).

When a bound variable is placed on the stack, it is immediately replaced by its value. To invoke the identifier and force delayed binding, the `&` or `@` sigil can be used. The `&` sigil indicates that the variable is statically bound in enclosing environment (Scheme-style static scope), whereas the `@` prefix invokes *automatic* scope, described below.

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

#### Scope

In `lviv`, all variables are statically scoped to *some* environment. However, because thunks must be defined before being bound by `lambda` or `let`, immediate static scoping is insufficient. *Automatic* or *delayed static* scoping allows variables in a thunk to be statically bound inside their enclosing `lambda` or `let`.

Static variables (invoked with `&`) bind immediately to the enclosing environment when the variable is put on the stack.

Automatic variables (invoked with `@`) behave like statically scoped variables, except that their binding is not fixed when they are put on the stack. Instead, they are bound when their enclosing thunk is passed to `lambda`, `let`, or `eval`, according to the following rules:

- If a `lambda` or `let` encloses an automatically scoped variable and the variable's identifier matches one of the `lambda` or `let` bindings, the variable is bound to the `lambda` or `let` environment.
- Otherwise, the variable becomes bound to the environment enclosing the `lambda`, `let`, or `eval`.

### Thunks

`lviv` represents delayed computations using thunks. The `eval` function attempts to apply a thunk, popping positionally referenced variables from the stack and binding them before evaluation. If the thunk references unbound variables, an error results from the computation and the stack is unmodified. `eval`ing a literal expression has no effect. If an error occurs during evaluation after the binding step, the modifications to the stack still occur and the value of the thunk becomes `nop`.

    > 1 eval
    1

#### Braced thunks

Thunks can be denoted by enclosing an expression in braces (`{}`).

    > { 1 0 / }
    #<thunk { 1 0 / }>
    > eval
    --> error: division by zero
    > 1 z define { 1 z + }
    #<thunk { 1 1 + }>
    > 2 z define eval
    2

#### Unbound identifiers

Referencing an unbound identifier in an expression results in a thunk where the unbound identifier is automatically scoped (as if invoked with `@identifier`). Statically scoped unbound identifiers can also be invoked using `&`.

    > x
    @x : #<unbound>
    > 1 +
    #<thunk { @x 1 + }>
    > 3 * &y -
    #<thunk { @x 1 + 3 * &y - }>
    > eval
    --> error: unbound variables x,y in eval
    #<thunk { @x 1 + 3 * &y - }>
    > @z /
    #<thunk { @x 1 + 3 * &y - @z / }>
    > 1 z define eval
    --> error: unbound variables x,y in eval
    #<thunk { @x 1 + 3 * &y - @z / }>

Note that when an unbound variable error occurs during an eval, all bindings are undone and the stack is unchanged.

#### Positional identifiers

Positional identifiers are identifiers of the form `#[0-9]+` which are unbound until evaluated. In a thunk, these identifiers represent the corresponding stack positions at the time the thunk is evaluated.

    > 1 #0
    1
    #0 : #<unbound>
    > eval
    1
    > #0 1 +
    1
    #<thunk { #0 1 + }>
    > 2 swap
    1
    2
    #<thunk { #0 1 + }>
    > eval
    1
    3

When thunks are nested, positional identifiers are relative to the immediately containing thunk. To escape nesting levels, prepend additional `#`s to the identifier.

	> 0 { { 1 ##0 / } 0 #0 == dropIf } eval

### Lambdas

`lambda` combines a thunk and a positional binding list into a function. Thunks using positional identifiers cannot be bound with a `lambda`.

Positional binding lists map variables inside the thunk to positional references on the stack. List elements are numbered from left to right starting at 0. Any automatically scoped variables in the thunk that correspond to identifiers in the positional binding list become lexically scoped to the `lambda`, resulting in a closure.

    > x
    @x : #<unbound>
    > 1 +
    #<thunk { @x 1 + }>
    > y *
    #<thunk { @x 1 + @y * }>
    > [y,x] lambda
    #<lambda [y,x] thunk { x 1 + y * }>
    > 2 1
    #<lambda [y,x] thunk { x 1 + y * }>
    2
    1
    > 3 roll
    2
    1
    #<lambda [y,x] thunk { x 1 + y * }>
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

The `let` expression uses a named binding list rather than a positional binding list. A named binding list is a list of tuples which are bound sequentially and then evaluated. In this way, `let` behaves much like Scheme's `letrec`: variables inside the named binding list may reference other variables in the list whether they are bound before or after. Like the `lambda`, automatically scoped variables in the thunk and the RHS of the named binding tuples that correspond to identifiers in the named binding list become lexically scoped to the `let`. This means that named bindings need not shadow variables in enclosing environments, since lexical bindings can be passed in.

    > 2 a define
    --> a : 2
    > { &a a b + * } [(a,1),(b,a &a +)] let
    8
    > z +
    #<thunk { 8 @z + }>
    > a * [(a,1)] let
    #<thunk { 8 &z + 1 * }>

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


