# TAPL in PureScript
The PureScript implementation for the *Types and Programming Language* by Benjamin C. Pierce

## Done
  - *Chapter 8: Typed Artihmetic Expression* (`TAPL.BoolNat.*`)
  - *Chapter 9: Simply Typed Lambda Calculus* (`TAPL.STLC.*`)

## WIP
  - *Chapter 11: Simple Extension* (`TAPL.STLCEx`).
     - [x] `unit` type 
     - [x] Ascriptions
     - [x] `let...in` expression
     - [x] Tuples 
     - [x] Records
     - [ ] Variant (ADT)      ... almost done
     - [ ] Pattern Matching   ... WIP
     - [x] General Recursions
     - [x] `let rec ... and` syntax
     - [ ] Lists

## Not Yet
  - *Chapter 13: References* 
  - *Chapter 14: Exceptions* 
  - *Chapter 15-16: Subtyping, algorithmic*
  - *Chapter 20-21: Recursive Types*
  - *Chapter 22: Type Reconstruction*
  - *Chapter 23: Universal Types (a.k.a. parametric polymorphism)* 
  - *Chapter 24: Existential Types*

## Syntax of STLCEx
### Types
```
type ::=                                ; types
         bool                           ; boolean
         nat                            ; natural numbers
         unit                           ; unit
         type -> type                   ; function
         {  l1: t1, l2: t2, ... }       ; record type
         {| l1: t1, l2: t2, ... |}      ; variant type
```

### Function 
The argument type annotation is mandatory
```
> fun (n:nat) -> isZero n 
< if = <fun>
     : nat -> bool
```

### Tuple & Record
Tuple type (product type) is represented by `*`.
```
nat * bool
nat * bool * unit 
```

Tuple values are comma-separated sequence of values wrapped in curly braces:
```
let tpl : (nat * bool) = { 42, true }
```

The syntax for record types:
```
{ foo : nat, bar : bool }
```

The syntax for record value:
```
let rcd : { foo: nat, bar: bool } = { foo = 42, bar = true }
```

### Variant 
Syntax for *:variant types** are comma-separated list of `label : type`,
wrappe in `{|` and `|}`.

e.g. This is `Maybe Boolean` type in PureScript:
```
{| nothing: unit, just:bool |}
```

Variant value construction:
```
let nat_op = {| some = 42 |} as {| none:unit, some:nat |}
```
Note that you cannot drop type ascription.

### Recursive functions
Use `let rec` syntax:
```
> let rec plus 
  : (nat -> nat) 
  = fun (m:nat) (n:nat) ->
      if isZero m then n 
      else succ (f (pred m) n)
  in plus 3 2

< it = 5
     : nat
```
Mutually recursive functions can be defined with
`let rec...and...` syntax.
```
> let rec even 
    : nat -> bool 
    = fun (n:nat) ->
        if isZero n then true
        else odd (pred n)
  and odd
    : nat -> bool
    = fun (n:nat) ->
        if isZero n then false 
        else even (pred n)
  in 
    even 5

< it = false 
     : bool 
```

Actually, The `let rec` is a syntax sugar and 
desugared into the form with `fix` primitive combinator during well-formedness checking:
```
let plus: nat -> nat -> nat = fix 
  (fun (f:nat -> nat -> nat) (m:nat) (n:nat) ->
     if isZero m then n 
     else succ (f (pred m) n)
  )
in plus 3 2
```

The desugared version of mutually recursive functions is a bit complicated.
```
let evenodd: { even: nat -> bool, odd: nat -> bool } = fix 
  (fun (eo:{ even: nat -> bool, odd: nat -> bool }) ->
    { even = fun (n:nat) -> 
        if isZero n then true 
        else eo#odd (pred n)
    , odd = fun (n:nat) ->
        if isZero n then false 
        else eo#even (pred n)
    }
  ) in 
let even : nat -> bool = evenodd#even in 
let odd : nat -> bool = evenodd#odd in 
even 5
```