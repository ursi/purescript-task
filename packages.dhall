let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

let additions =
      https://raw.githubusercontent.com/ursi/purescript-package-set/dev/packages.dhall sha256:7ea09e6ea132b449cbcb051066a1c0e3790ed4638a28c18028908ac35b1caac6

in  upstream // additions
