{ ps-pkgs, ps-pkgs-ns, licenses, ... }:
  with ps-pkgs;
  { version = "0.3.0";
    dependencies = [ js-timers ps-pkgs-ns.ursi.prelude ];

    pursuit =
      { name = "task";
        repo = "https://github.com/ursi/purescript-task.git";
        license = licenses.bsd3;
      };
  }
