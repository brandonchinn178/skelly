#!/usr/bin/env python3
# ruff: disable[F541]

from __future__ import annotations

import itertools
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, NamedTuple

import kdl

HERE = Path(__file__).resolve().parent
TOP = HERE.parent
BUILD_DIR = HERE / "build"
DIST_DIR = HERE / "dist"
SRC_DIR = TOP / "src"


def main():
    if sys.argv[1:2] == ["test"]:
        test(sys.argv[2:])
    else:
        build()


def init_build_dir():
    BUILD_DIR.mkdir(parents=True, exist_ok=True)
    if not (BUILD_DIR / "src").exists():
        (BUILD_DIR / "src").symlink_to(TOP / "src")

    DIST_DIR.mkdir(parents=True, exist_ok=True)

    hs_package = HsPackage.parse(TOP / "hspackage.kdl")
    hs_package.write_cabal(TOP / "skelly.cabal")
    (TOP / "skelly.cabal").copy(BUILD_DIR / "skelly.cabal")

    ghc_version = hs_package.find_appropriate_ghc_version()
    subprocess.run(
        [
            "cabal",
            "configure",
            "-w",
            f"ghc-{ghc_version}",
            "--enable-tests",
            "--test-show-details=streaming",
            "--ghc-options=-fdiagnostics-color=always",
        ],
        cwd=BUILD_DIR,
        check=True,
    )


def get_cabal() -> str:
    cabal = shutil.which("cabal")
    if not cabal:
        raise Exception("cabal not found")
    return cabal


def build():
    init_build_dir()
    os.chdir(BUILD_DIR)
    cabal = get_cabal()
    os.execv(
        cabal,
        [
            "cabal",
            "install",
            "--install-method=symlink",
            "--overwrite-policy=always",
            f"--installdir={DIST_DIR}",
        ],
    )


# TODO: remove when `skelly test` works
def test(args):
    init_build_dir()

    shutil.rmtree(BUILD_DIR / "test", ignore_errors=True)
    test_modules = []
    for f in (TOP / "src").rglob("*.spec.hs"):
        path = f.relative_to(TOP / "src")
        module_name = path.as_posix().replace(".spec.hs", "").replace("/", ".")

        dest = BUILD_DIR / "test" / path
        dest = dest.with_stem(dest.stem.replace(".spec", "Spec"))

        test_module_name = module_name + "Spec"
        test_file = f.read_text()
        test_file_lines = test_file.splitlines()
        body_start_line = len(
            list(
                itertools.takewhile(
                    lambda s: re.match(r"{-#|\s*$", s) is not None, test_file_lines
                )
            )
        )
        test_file_lines = [
            *test_file_lines[:body_start_line],
            f"module {test_module_name} where",
            f"import Skeletest",
            *(
                ["import qualified Skeletest.Predicate as P"]
                if re.search(r"\bP\.\w", test_file)
                else []
            ),
            *(
                ["import qualified Skeletest.Prop.Gen as Gen"]
                if re.search(r"\bGen\.\w", test_file)
                else []
            ),
            *(
                ["import qualified Skeletest.Prop.Range as Range"]
                if re.search(r"\bRange\.\w", test_file)
                else []
            ),
            f"import {module_name}",
            f'{{-# LINE {body_start_line + 1} "{f.relative_to(TOP)}" #-}}',
            *test_file_lines[body_start_line:],
        ]

        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_text("\n".join(test_file_lines))

        test_modules += [test_module_name]

    (BUILD_DIR / "test" / "Main.hs").write_text("import Skeletest.Main")

    cabal_lines = [
        "test-suite skelly-test",
        "  type: exitcode-stdio-1.0",
        "  hs-source-dirs: test",
        "  ghc-options: -Wall -Werror -F -pgmF=skeletest-preprocessor",
        "  build-tool-depends: skeletest:skeletest-preprocessor",
        "  default-language: GHC2024",
        "  main-is: Main.hs",
        "  other-modules:",
        *(f"    {s}" for s in test_modules),
        "  build-depends:",
        "    base,",
        "    containers,",
        "    skelly,",
        "    skeletest,",
        "    text,",
        "    unliftio,",
    ]
    cabal = (BUILD_DIR / "skelly.cabal").read_text()
    (BUILD_DIR / "skelly.cabal").write_text(
        cabal + "\n" + "\n".join(cabal_lines) + "\n"
    )

    os.chdir(BUILD_DIR)
    cabal = get_cabal()
    os.execv(
        cabal,
        ["cabal", "test", *(f"--test-option={arg}" for arg in args)],
    )


class HsPackage(NamedTuple):
    config: dict[str, Any]

    @classmethod
    def parse(cls, file: Path) -> HsPackage:
        config = kdl.parse(file.read_text())
        return cls(config=config)

    # TODO: Replace with `skelly gen-cabal`
    def write_cabal(self, file: Path) -> None:
        # header + metadata
        name = self.config["package"]["name"].args[0]
        version = self.config["package"]["version"].args[0]
        lines = [
            f"cabal-version: 2.4",
            f"name: {name}",
            f"version: {version}",
        ]

        # library
        modules = self.get_modules()
        lib_deps = [
            f"{dep.name} {_rewrite_range(dep.args[0])}"
            for dep in self.config["package"]["dependencies"].nodes
        ]
        lines += [
            f"library",
            f"  default-language: GHC2024",
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
            f"  default-language: GHC2024",
            f"  main-is: src/Main.hs",
            f"  ghc-options: -Wall -Werror",  # TODO: add this dynamically
            f"  build-depends:",
            f"    {name},",
            *(f"    {dep}," for dep in lib_deps),
        ]

        file.write_text("\n".join(lines))

    def is_allowed_ghc_version(self, version: str) -> bool:
        version_info = tuple(int(x) for x in version.split("."))
        # TODO: parse bounds from self.config["skelly"]["toolchain"]["ghc"]
        return version_info >= (9, 10) and version_info < (9, 12)

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

        raise Exception("Could not find a compatible GHC")

    def get_modules(self) -> list[str]:
        modules = []

        for f in SRC_DIR.rglob("*.hs"):
            f = f.relative_to(SRC_DIR)

            if f.name == "Main.hs":
                continue
            if f.suffixes[0] == ".spec":
                continue

            modules.append(".".join([*f.parts[0:-1], f.stem]))

        return modules


def _rewrite_range(range: str) -> str:
    if range.startswith("^"):
        return f"^>= {range[1:]}"
    else:
        return range


if __name__ == "__main__":
    main()
