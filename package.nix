{ ps-pkgs, ps-pkgs-ns, ... }:
  with ps-pkgs;
  { version = "0.3.0";
    dependencies = [ js-timers ps-pkgs-ns.ursi.prelude ];
  }
