let testDependencies = [ "ansi", "assert", "js-timers" ]

in  { name = "task"
    , dependencies = [ "mason-prelude" ] # testDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    , license = "BSD-3-Clause"
    , repository = "https://github.com/ursi/purescript-task.git"
    }
