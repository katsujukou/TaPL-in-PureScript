# TAPL in PureScript
The PureScript implementation for the *Types and Programming Language* by B. Pierce

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
     - [ ] `let rec ... and` syntax
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
         {  l1: t1, l2: t2, ... }       ; record type
         {| l1: t1, l2: t2, ... |}      ; variant type
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
{| nothing: unit, just:nat |}
```

Variant value construction:
```
let nat_op = {| some = 42 |} as {| none:unit, some:nat |}
```
Note that you cannot drop type ascription.

### Recursive function
Use `fix` primitive:
```
> let 
  plus = fix (
    fun (f: nat -> nat -> nat) (m:nat) (n:nat) ->
      if isZero m then n 
      else succ (f (pred m) n)
  )
in 
  plus 3 2

< it = 5
     : nat
```