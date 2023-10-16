#!/usr/bin/env python3

from __future__ import annotations

import sys
if sys.version_info < (3, 11):
    raise Exception("Bootstrap build requires at least Python 3.11")

import subprocess
import tomllib
from pathlib import Path
from typing import Any, NamedTuple

HERE = Path(__file__).resolve().parent
TOP = HERE.parent
BUILD_DIR = HERE / "build"
DIST_DIR = HERE / "dist"
SRC_DIR = TOP / "src"

def main():
    BUILD_DIR.mkdir(parents=True, exist_ok=True)
    if not (BUILD_DIR / "src").exists():
        (BUILD_DIR / "src").symlink_to(TOP / "src")

    DIST_DIR.mkdir(parents=True, exist_ok=True)

    hs_package = HsPackage.parse(TOP / "hspackage.toml")
    hs_package.write_cabal(BUILD_DIR / "skelly.cabal")

    ghc_version = hs_package.find_appropriate_ghc_version()
    subprocess.run(
        [
            "cabal",
            "install",
            "-w", f"ghc-{ghc_version}",
            "--overwrite-policy=always",
            "--install-method=symlink",
            f"--installdir={DIST_DIR}",
        ],
        cwd=BUILD_DIR,
        check=True,
    )

class HsPackage(NamedTuple):
    config: dict[str, Any]

    @classmethod
    def parse(cls, file: Path) -> HsPackage:
        with file.open("rb") as f:
            config = tomllib.load(f)
        return cls(config=config)

    def write_cabal(self, file: Path) -> None:
        # header + metadata
        name = self.config["package"]["name"]
        version = self.config["package"]["version"]
        lines = [
            f"cabal-version: 2.4",
            f"name: {name}",
            f"version: {version}",
        ]

        # library
        modules = self.get_modules()
        lib_deps = [
            f"{name} {version_range}"
            for name, version_range in self.config["dependencies"].items()
        ]
        lines += [
            f"library",
            f"  default-language: Haskell2010",
            f"  default-extensions: ImportQualifiedPost",
            f"  hs-source-dirs: src",
            f"  ghc-options: -Wall -Werror",  # TODO: add this dynamically
            f"  exposed-modules:",
          *(f"    {s}" for s in modules),
            f"  build-depends:",
          *(f"    {dep}," for dep in lib_deps),
        ]

        # executable
        lines += [
            f"executable {name}",
            f"  default-language: Haskell2010",
            f"  default-extensions: ImportQualifiedPost",
            f"  main-is: src/Main.hs",
            f"  ghc-options: -Wall -Werror",  # TODO: add this dynamically
            f"  build-depends:",
            f"    {name},",
          *(f"    {dep}," for dep in lib_deps),
        ]

        file.write_text("\n".join(lines))

    def is_allowed_ghc_version(self, version: str) -> bool:
        version_info = tuple(int(x) for x in version.split("."))
        # TODO: parse bounds from self.config["toolchain"]["ghc"]
        return version_info >= (9, 6) and version_info < (9, 8)

    def find_appropriate_ghc_version(self) -> str:
        ghcup_list = subprocess.run(
            ["ghcup", "list", "-r", "-c", "installed"],
            check=True,
            capture_output=True,
            encoding="utf-8",
        )
        for line in ghcup_list.stdout.split("\n"):
            parts = line.split(" ")
            if parts[0] != "ghc":
                continue
            version = parts[1]
            if self.is_allowed_ghc_version(version):
                return version

        raise Exception(
            f"Could not find an installed GHC matching {self.ghc_toolchain}"
        )

    def get_modules(self) -> list[str]:
        modules = []

        for f in SRC_DIR.rglob("*.hs"):
            f = f.relative_to(SRC_DIR)

            if f.name == "Main.hs":
                continue

            modules.append(".".join([*f.parts[0:-1], f.stem]))

        return modules

if __name__ == "__main__":
    main()
