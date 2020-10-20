let testDependencies = [ "assert", "js-timers" ]

in  { name = "my-project"
    , dependencies = [ "exceptions", "mason-prelude" ] # testDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
