# SymFix

This repository contains a prototype of SymFix for Rosette. See the full paper at https://unsat.cs.washington.edu/papers/porncharoenwase-symfix.pdf.

To install SymFix, clone this repo and run `raco pkg install` inside the directory.

To run SymFix, execute `raco symfix <path-to-a-rosette-program>`. Binary files of extension `.symfix` will be generated along with diff files of extension `.symfix-diff`. For instance, a Rosette program `a.rkt` would result in `a.rkt.symfix` and `a.rkt.symfix-diff`. There are two ways to apply generated repairs to `a.rkt`.

- Simply overwrite `a.rkt` with `a.rkt.symfix`. This is useful for verification that the repair actually works, but might not be desirable in a long run as the file is binary.
- Manually extract repairs from `a.rkt.symfix-diff` and apply the repairs to `a.rkt`. This is more desirable, but also more tedious. Due to the fact that Rosette is implemented in Racket which has syntactic extensions, we need to fully expand the program first to understand its semantics. However, this means that the diff of expanded programs might not resemble the unexpanded programs.
