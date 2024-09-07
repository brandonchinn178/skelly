# Design

This document provides an overview of what Skelly does, under the hood.

## PackageIndex

A "package index" is an abstraction for anything that provides a registry of Haskell packages. For the most part, this is simply Hackage, but this could conceptually be swapped out by another index: a database, a directory of files, etc. See the `PackageIndex` type for the interface a package index must implement.

Package indexes can cache files at `~/.cache/skelly/package-index/`.

## Build

Unlike Cabal, which builds libraries and executables as separate units of compilation, Skelly effectively treats everything as one giant library. Say you have the following project:

* `dep`
    * Library: `Dep.hs`, `Dep/Feature1.hs`, `Dep/Feature2.hs`
* `foo`
    * Library: `Foo.hs`, `Foo/Bar.hs`
    * Executables: `prog1.hs`, `prog2.hs`
    * Depends on `dep`

Cabal would build the `dep:lib` target first, then the `foo:lib` target, then the `foo:exe:prog1` and `foo:exe:prog2` targets. But if `Foo/Bar.hs` only depends on `Dep.Feature1` and `prog2.hs` only depends on `Foo.Bar`, there's a lot of wasted cycles you could parallelize.

Instead, if you tell Skelly to build everything, Skelly does the following:
1. Rewrite `prog1.hs` as `Main_prog1.hs` (same for `prog2.hs`)
2. Add a new `prog1.hs` file that just defines `main = Main_prog1.main`
3. Build all the files in batch mode + in parallel:
    ```bash
    ghc -j Dep.hs Dep/Feature1.hs Dep/Feature2.hs Foo.hs Foo/Bar.hs Main_prog1.hs Main_prog2.hs
    ```
4. Build the new entrypoint files (should be fast, since the object files were just built)
    ```bash
    ghc prog1.hs
    ```

## GHC

TODO: `dist/...`
