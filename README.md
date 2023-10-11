# Skelly

Skelly is an opinionated build system for Haskell.

Features:
* TOML configuration
* Standardized file structure
* Built-in test runner
* CLI-driven workflow

## Roadmap

MVP:
* `skelly clean`
* `skelly build`
    * Build third-party deps with Cabal build (shim)
    * Dependencies with syntax: `>= 1 && < 2`, `1.2.3` (pinned version)
    * Lock file
* `skelly run`
* `skelly install`
    * Install executables to `~/.local/bin/`
* `skelly test`
    * Built-in test framework
* `skelly add` to add dependencies (and other similar commands)
* GHC options

V1:
* Workspace support (i.e. monorepos)
* `skelly doc`
* `skelly sdist`
* `skelly fmt` - bless fourmolu with specific config
* Specify dep from git repo

V2:
* `skelly bench`
* User-defined scripts
* `.skelly/config.toml`
* C libraries
* CPP support
* Flags + other conditional stuff
* Share hsproject config (e.g. in a monorepo)

Long shot:
* New package repository
    * Hackage for Skelly
    * New sdist manifest format (no more cabal files)
    * No docs
* New docs site
    * Generate intermediate data format with docs and type signatures
    * Generate docs from intermediate data format
        * Decouples docs generation from compiler
        * Docs for an old version of a package uses latest styling + functionality
