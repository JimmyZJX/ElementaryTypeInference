# WorklistTopBot

## Build from Scratch

This project can be built with [Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack build
stack exec WorklistTopBot-exe <path>
```

## Quick Reference

* Types: `Int`, `Bool`, `Top`, `Bot`, `forall a. Type`, `Type -> Type`, `[Type]`
* Int literals: `0`, `1`, `2` ...
* Bool literals: `True` / `False`
* Lambda: `\x -> x`
* Fixpoint: `fix \x -> x`
* Application: `(\x -> x) 1`
* Type annotation: `1 :: Int`
* Type application: `((\x -> x) :: forall a. a -> a) @Int 3`
* Type abstraction: `(/\a. \x -> x) :: forall a. a -> a`
* List: `[]` / `1 : 2 : 3 : []` / `True : False : []` ...
* Case: `case lst of [] -> []; (x :: xs) -> ...`
* Let: `let x = 1 in \y -> x` / `let id :: forall a. a -> a = \x -> x in id @Int 3`

## Quick notes on the implementation

We implemented all the algorithmic rules in the paper. In addition, to make the
examples more interesting we have also implemented a few more simple extensions:

- Polymorphic lists ([a]) and a case analysis expression for
pattern matching on lists;
- Recursion via a fixpoint operator;
- Recursive let expressions

All the examples provided in the paper run in our implementation. 

## Examples

See the [examples/](./examples/) directory. Here is an interesting example:

```
let map :: forall a. forall b. (a -> b) -> [a] -> [b] =
    /\ a. /\ b. \f -> \xs -> case xs of
                                 [] -> [];
                                 (y : ys) -> f y : map f ys
    in
        let plus = \x -> \y -> 1 in
            let succ = plus 1 in
                map succ (1 : 2 : [])

```

In this example we create the map function on lists, and then apply
it in `map succ (1 : 2 : [])`.