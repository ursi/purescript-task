{ name = "my-project"
, dependencies = [ "exceptions", "mason-prelude", "parallel" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
