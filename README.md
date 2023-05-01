# Mono
Inductive Synthesis of Functional Programs - Arjun Sampath and Alexander Yu

Inspired by [Trio](https://github.com/pslhy/trio) and implemented in Rust, Mono is a tool for the inductive synthesis of functional programs given a set of input out examples, custom data types, and a type signature, generating a satisfying recursive function in return

Mono synthesizes programs primarily requiring boolean and non-recursive list inputs - we were unable to evaluate Mono on programs with BSTs, expression generation, and recursive lists.  Benchmarks were taken from [Trio](https://github.com/pslhy/trio).  Further work includes more consistent synthesis of recursive functions and utilizing [equivalence graphs](https://egraphs-good.github.io/) instead of standard observational equivalence

Implementation and reflection can be found in our [slides](https://docs.google.com/presentation/d/1mxlQd_Sg1xPSqJp_vW99XIbWP2MobNB2E-qIPEcDME4/edit#slide=id.p)

## Demo
Test benchmark files are within the ```proj_root/src``` directory, modify the test name in ```main.rs``` ex. ```mod bool_impl;``` and ```specification.rs``` ex. ```use crate::bool_impl::get_declarations```

    cargo run
