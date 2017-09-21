# stateflow_CPS_semantics
A continuation passing style semantics for Stateflow in Ocaml

Software implements:
- our generic CPS semantics, instanciated as
  - an evaluator (aka simulator)
  - an imperative code generator
  - a lustre automaton generator compatible with lustrec (github/coco-team/lustrec)
- the reference denotational semantics of Stateflow by Hamon as presented in his EMSOFT'05 paper.

Options:
sf_sem takes as input a model name and a backend. Traces are hard coded in the source files.

Backends are interpretor, imperative code generator, lustre code generator.
  -model model in {simple, stopwatch} (default: simple)
  -eval execute the model on trace <int>
  -eval-mode select evaluator: cps, emsoft05 or compare
  -gen_imp generate imperative code
  -gen_lustre generate lustre model
  -modular generate modular code (either for imperative or lustre backend) 0 is not modular, 1 modularize nodes, 2 modularize entry, during and exit actions (default 0)

Example:
./sf_sem -model stopwatch -eval-mode compare -eval 1

