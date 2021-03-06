# Elementary Type Inference (Artifact)

Title of the submitted paper: Elementary Type Inference

ECOOP submission number for the paper: #8

## Overview

* `src/proof_2.0.8_dev` contains all Abella proof scripts that run in the unmodified version of Abella (Abella 2.0.8-dev or later). This
should be enough to check our proof claims.
* (Optional part) `src/proof_modded` contains the Abella proof script that we actually wrote in a modifed version of Abella, which
includes some extra tactics not supported in Abella (those are shorter than the proofs in the unmodifed version).
* (Optional part) `src/abella_src_modded` contains our modified version of Abella, for those wishing to build and check our original proofs.
* `src/implementation` contains a simple Haskell implementation of our type inference algorithm that can run the examples in Section 2.5.
The implementation will print the algorithmic derivation rules employed during the inference process. 

We claim all three badges including **functional, reusable, and available badges**.

The paper has claimed that we formalized the corrrectness of a type inference algorithm for a variant of
Fsub (without bounded quantification). The 3 main results are: soundness, completness and decidability of the
type-inference algorithm. Moreover there are several proofs about the declarative system and also
a soundness/completeness proof between stable subtyping and a syntax-directed specification.
In the appendix in page 41 of the submission, there is a table that describes the correspondence between
notations in the paper and the corresponding files and names used in the Abella formalization.
We also describe the correspondence between key lemmas in Abella and the paper below.

In addition, this Abella proof artifact is also reusable: any extension
or alteration to the system can be easily done with our provided Abella scripts,
and after possible adjustments, all the proofs can be
reused for the formalization of the extended or altered system.

We agree to publishing our artifact under a Creative Commons license.

## Artifact Requirements

### Proofs

#### Required Version (for proofs in `src/proof_2.0.8_dev`)


Abella v2.0.8-dev, commit `d3a2d56` or above
in the master branch of its GitHub repository: <https://github.com/abella-prover/abella>.

### Implementation

The implementation can be built with the GHC Haskell compiler and Stack.

## Getting Started

Our VM image already contains all the artifacts built, but the artifacts can
also be built from scratch. We describe the instructions to build from scratch as well next.

### Proofs

#### Required Version (for proofs in `src/proof_2.0.8_dev`)


Abella v2.0.8-dev, commit `d3a2d56` or above
in the master branch of its GitHub repository: <https://github.com/abella-prover/abella>.

#### How to Build Abella


The official description is in the *Download* section of its project webpage: <http://abella-prover.org>.

But we give another step-by-step instruction here, in case the reader is not familiar with OCaml.

- A Linux machine is preferred due to the convenience on package management
- Install OCaml with OPAM: `apt install opam` on Ubuntu, or refer to <https://opam.ocaml.org/doc/Install.html>
- Install [findlib](http://projects.camlcity.org/projects/findlib.html)
    and [ocamlbuild](https://github.com/ocaml/ocamlbuild)
    via opam: `opam install ocamlbuild ocamlfind`
- Build and install Abella (master branch of the git repo: <https://github.com/abella-prover/abella>) with `make`, and add to PATH or move to anywhere in PATH
- Run the Abella executable and you should see: `Welcome to Abella 2.0.8-dev.` (Then type `Quit.` to quit)
- (Optional) Configure Proof-General for Abella: <https://github.com/abella-prover/PG>

How to Compile/Re-check the Proofs
------
- Execute `make` in the `src` folder to check the proof scripts in the `proof_2.0.8_dev` folder, which targets the official Abella 2.0.8 dev version. (This might take one minute to finish)
- (Optional) Build the modded version of abella (`make` in the `abella_src_modded` folder), install its build binary, and execute `make proof_modded` to check proof scripts in the `proof_modded` folder.
- The shell command `grep -r "skip" proof*/` further confirms that no theorems/lemmas are assumed but not proven.
- (Optional) The Abella plugin in SublimeText is installed on the VM. First, open some Abella source file `*.thm`; then use `Ctrl+Enter` to start the Abella process in background. Navigate through `Ctrl+Up`, `Ctrl+Down`, `Ctrl+Enter` (to the cursor). Finally shutdown with `Ctrl+Shift+Enter`.
- The user name and password of the VM image are both "elementary".

Explore Around the Proofs
------
- We provide two versions of our formalization, mainly because we use a modded version to develop. Therefore we provide a version that checks in the official Abella as well. `proof_modded` contains our original proof, and `proof_2.0.8_dev` is generated by a mixture of automatic translation and manual fixes.
- The reader is expected to be familiar with Abella or similar system with HOAS representations. Experiences on other proof assistants definitely help, but the proofs might look different and thus difficult to understand some critical details.
- `rules.thm` is the starting point that defines all the types, terms, declarative rules and algorithmic rules.
- `nonOverlap.thm` argues the equivalence of the original declarative system and the non-overlapping variant, which contains two side conditions that can be omitted as we mentioned in the paper.
- `trans.thm` defines worklist instantiation(`tex`) and declarative transfer (`dc`).
- `soundness.thm` contains a lot of uninteresting lemmas on the shape invariance during worklist instantiation.
- `dcl.thm` introduces another equivalent definition `dcl` to declarative transfer `dc`, which encodes the non-overlapping declarative worklist with a step-by-step style, used for the induction of the complteness theorem.
- `completeness.thm` also contains a lot of uninteresting lemmas as soundness do.
- `inst_decidable.thm` and `decidability.thm` proves the termination results
- `stableSub.thm` and `safety.thm` include meta-theory of stable subtyping and our restricted Fsub, respectively.

### Proof Structure

* `syntax_ott.v`: all definitions, functions, and inductively defined relations, generated by ott
* `rules_inf.v`: infrastructure proofs regarding [locally nameless representation](), generated by [lngen]()
* `LibTactics.v`: useful tactics maintained by Arthur Chargueraud, available at [LibTactics]()
* `Infrastructure.v`: infrastructure lemmas based only on the syntax and well-formedness
* `TypeLemmas.v`: lemmas about ordinary types, splittable types, proper types
* `Subtyping.v`: lemmas about subtyping
* `Disjointness.v`: lemmas about disjointness
* `IsomorphicSubtyping.v`: lemmas about *Isomorphic Subtyping* and *Applicative Distributivity* including binding related ones
* `TermLemmas.v`: lemmas about regularity and substitution on typing and lemmas about values and prevalues
* `KeyProperties.v`: lemmas about *principal type* and typing
* `Progress.v`: lemmas about determinism and progress, also includes lemmas about *Casting* and *Consistency*
* `TypeSafety.v`: lemmas about preservation, also includes lemmas about typing
* `ConsistencySpecification.v`: lemmas about soundness of consistency

## Implementation

### Building from Scratch (under directory `src/implementation`)

You need to install GHC first if you wish to build the implementation from scratch:

https://www.haskell.org/downloads/

This project can be built with [Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack build
stack exec WorklistTopBot-exe <path>
```

### Quick Reference

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

### Quick notes on the implementation

We implemented all the algorithmic rules in the paper. In addition, to make the
examples more interesting we have also implemented a few more simple extensions:

- Polymorphic lists ([a]) and a case analysis expression for
pattern matching on lists;
- Recursion via a fixpoint operator;
- Recursive let expressions

All the examples provided in the paper run in our implementation. 

### Examples

See the [examples/](./examples/) directory. Here is an interesting example:

```
let map :: forall a. forall b. (a -> b) -> [a] -> [b] =
    \f -> \xs -> case xs of
                                 [] -> [];
                                 (y : ys) -> f y : map f ys
    in
        let plus = \x -> \y -> 1 in
            let succ = plus 1 in
                map succ (1 : 2 : [])

```

In this example we create the map function on lists, and then apply
it in `map succ (1 : 2 : [])`.

## Correspondence between paper and Abella proofs

### Definitions

The table in page 41 of the submission illustrate the correspondence between our definitions
and the Abella files/definitions.

### Lemmas, Theorems, and Corollaries


| Paper            | File                    | Name in Abella                         |
|------------------|-------------------------|----------------------------------------|
| Lemma 1          | `nonOverlap.thm`        | `Theorem chk_subsumption`              |
| Corollary 2      | `nonOverlap.thm`        | `Theorem sub_subst_wft` (generalized)  |
| Lemma 3          | `declarative.thm`       | `Theorem sub_refl`                     |
| Lemma 4          | `nonOverlap.thm`        | `Theorem sub_trans`                    |
| Theorem 5        | `stableSub.thm`         | `Theorem sub_sound_wrt_stable`         |
| Theorem 6        | `stableSub.thm`         | `Theorem sub_complete_wrt_stable`      |
| Lemma 7          | `nonOverlap.thm`        | `Theorem sub_subst_wft`                |
| Lemma 9          | `nonOverlap.thm`        | `Theorem subsumption_thm`              |
| Lemma 10         | `nonOverlap.thm`        | `Theorem sub_sty_ty`                   |
| Lemma 11         | `safety.thm`            | `Theorem sub_sound_fsub`               |
| Theorem 12       | `safety.thm`            | `Theorem typ_sound_fsub`               |
| Lemma 13         | `safety.thm`            | `Theorem sub_complete_fsub`            |
| Theorem 14       | `safety.thm`            | `Theorem typ_complete_fsub`            |
| Theorem 16       | `soundness.thm`         | `Theorem soundness`                    |
| Theorem 17       | `completeness.thm`      | `Theorem completeness`                 |
| Lemma 18         | `depth.thm`             | `Theorem prune_tex_instL`              |
| Theorem 19       | `decidable.thm`         | `Theorem decidable`                    |
| Corollary 20     | `decidable.thm`         | `Theorem decidable_decl`               |
