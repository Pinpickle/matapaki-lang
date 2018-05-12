# Matapaki

A statically typed, functional programming language for the Ethereum Virtual Machine. Built for university dissertation.

Features:

- Solidity-compatible ABI
- Free of re-entrancy and unchecked-send
- Formal semantics
- Persistent state
- Records
- Mappings (as temporary variables and in state)

## Set Up

Make sure you have these installed:

- Isabelle 2017
- OCaml v4.02.3

For testing, you will need

- Node.js v6+
- A native Solidity compiler (solc) installed

Once the repository is downloaded, extract [eth-isabelle]() into `compiler/backend` (so it becomes `compiler/backend/eth-isabelle`). Follow the instructions in its README to set it up.

Run `yarn` to download testing packages.

Note: When building, jbuilder will likely complain about missing packages. Run `opam install x` to install them.

## Building

Open `compiler/backend/Compiler.thy` in Isabelle and wait for it to export the theory as OCaml (it will be in `compiler/frontend/compiler_theory.ml`). Every time you edit the back-end, you'll have to do this.

Run `yarn build`

## Testing

After testing, you can run `yarn test` to run the unit test suite.

To run experiments and tests against the corpus, run `node ./testing/corpus/export_results.js`. The results of experiments will be in an `experiments` folder at the top level.