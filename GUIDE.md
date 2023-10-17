# User Guide

TODO: Turn this doc into a website

Skelly is an opinionated build system and workflow runner. It aims to make common workflows and patterns easy.

## Overview

The most common thing you'll use Skelly for is building your code and running tests.

```bash
skelly build

# Build libraries and tests
skelly build --libs --tests
```

```bash
skelly test

# Run a test file matching the given pattern
skelly test src/Foo/Bar.spec.hs
```

Skelly, by default, expects packages to be laid out in the following manner:

```
.
├── skelly.lock
├── hspackage.toml
├── src/
│   ├── Main.hs
│   ├── MyLibrary.hs
│   ├── MyLibrary.spec.hs
│   └── MyLibrary/
│       ├── Cache.hs
│       └── Cache.spec.hs
└── tests/
    ├── MyIntegrationTest.hs
    └── MyIntegrationTest2/
        ├── Main.hs
        ├── Tests1.hs
        └── Tests2.hs
```

## Package configuration

`hspackage.toml` is the configuration for the package.

```toml
[skelly.package]
name = "my-package"
version = "0.0.0"

# Unnamed [[skelly.lib]] section has same name as package
[[skelly.lib]]
[skelly.lib.dependencies]
bar = "*"

[[skelly.lib]]
name = "another-lib"
[skelly.lib.dependencies]
bar = "*"

# Unnamed [[skelly.bin]] section has same name as package
[[skelly.bin]]
[skelly.bin.dependencies]
baz = "*"

[[skelly.bin]]
name = "my-exe"
[skelly.bin.dependencies]
baz = "*"
```

`hsproject.toml` is the configuration for the local project. The package configuration only contains configuration relevant for the package as a distributable unit, while the project configuration contains configuration relevant for the development workflow.

```toml
# Register custom scripts to run with 'skelly my-script'.
[skelly.scripts]
my-script = "..."

# Extra dependencies to install for development,
# categorized however makes sense for you
[[skelly.dev-dependencies.group.lint]]
hlint = "*"
stan = "*"
```

All of the sections in `hspackage.toml` and `hsproject.toml` are prefixed with `skelly` so that other tools can be configured from these files as well.

## Tests

### Unit tests

Unit tests are specified in a `.spec.hs` file colocated with the module being tested. All definitions in the module, even non-exported ones, are available in the test file without importing.

Unit tests use the `skelly-test` framework, and automatically includes the following imports, which puts all test definition identifiers (like `Spec`, `describe`, or `shouldBe`) into scope.

```hs
import Skelly.Test
import Skelly.Test.Predicate qualified as P
import Skelly.Test.Prop.Gen qualified as Gen
import Skelly.Test.Prop.Range qualified as Range
```

Unit tests look something like this:

```hs
-- MyLibrary/Cache.spec.hs

spec :: Spec
spec = do
  describe "cacheFunc" $ do
    it "returns True when input is 1" $ do
      assert (P.== True) $ cacheFunc 1

    prop "returns False when input is > 10" $ do
      x <- gen $ Gen.int $ Range.between (11, 1000)
      assert (P.== False) $ cacheFunc x

  describe "complexFunc" $ do
    it "thing" $ do
      assert (P.not . P.any (P.== 10)) $ complexFunc 1 True

    it "matches a snapshot" $ do
      assert P.matchesSnapshot $ complexFunc 10 False
```

### Integration tests

TODO

## Package management

### Dependency specification

TODO: `skelly add`, `skelly remove`


TODO: shorthand
```toml
[dependencies]
foo = "*"

# equivalent to:
[[lib]]
[lib.dependencies]
foo = "*"
[[bin]]
[bin.dependencies]
baz = "*"
```

### `skelly.lock`

Skelly uses a lock file named `skelly.lock` that pins the versions to use when building. This is most useful for building applications, where everyone should be using the same, pinned versions of all dependencies, to get reproducible builds. `skelly.lock` should ALWAYS be checked into your VCS.

`skelly build` will automatically create this file if it doesn't already exist. Adding and removing dependencies with `skelly add` or `skelly remove` will update the lock file.

If you've manually edited your dependencies, you can run `skelly lock`, which will make the minimum modifications needed to update the lock file. To update the lock file with the latest version of dependencies allowed by your bounds, you can run `skelly lock --upgrade`.

### Testing version bounds

If you're building a library, you'll still get a `skelly.lock` file, to ensure reproducible builds in development (and you'll want to commit this file also). But you'll also want to test that your library works for _all_ the bounds you've specified in your dependencies.

Skelly commands also take in a `--unlock` flag, which will 

## Workflow

TODO: `skelly fmt`
TODO: `skelly my-script`
