{ ps-pkgs, ps-pkgs-ns, ... }:
  with ps-pkgs;
  { version = "0.3.0";
    repo = "https://github.com/ursi/purescript-task.git";
    rev = "18ab410a5b6042f73bb47f792fc327e89e9a210c";
    dependencies = [ js-timers ps-pkgs-ns.ursi.prelude ];
  }
