let testDependencies = [ "ansi", "assert" ]

in  { name = "task"
    , dependencies = [ "js-timers", "mason-prelude" ] # testDependencies
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs", "test/**/*.purs" ]
    , license = "BSD-3-Clause"
    , repository = "https://github.com/ursi/purescript-task.git"
    }
