{ ps-pkgs, ps-pkgs-ns, ... }:
  with ps-pkgs;
  { version = "0.7.0";
    repo = "https://github.com/ursi/purescript-task.git";
    rev = "";
    dependencies = [ js-timers ps-pkgs-ns.ursi.prelude ];
  }
