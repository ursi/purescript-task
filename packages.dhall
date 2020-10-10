let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

let additions =
      https://raw.githubusercontent.com/ursi/purescript-package-set/5/packages.dhall sha256:2b8a4cfa83745d7f2de570bf8340207e8516f873dbc103efbaabf1b058324720

in  upstream // additions
