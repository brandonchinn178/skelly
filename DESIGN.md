# Design

This document provides an overview of what Skelly does, under the hood.

## PackageIndex

A "package index" is an abstraction for anything that provides a registry of Haskell packages. For the most part, this is simply Hackage, but this could conceptually be swapped out by another index: a database, a directory of files, etc. See the `PackageIndex` type for the interface a package index must implement.

Package indexes can cache files at `~/.cache/skelly/package-index/`.

## GHC

TODO: `dist/...`
