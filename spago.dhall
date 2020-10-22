let testDependencies = [ "ansi", "assert", "js-timers" ]

in  { name = "task"
    , dependencies = [ "mason-prelude" ] # testDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    }
