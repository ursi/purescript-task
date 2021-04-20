{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url = "github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils";
    };

  outputs = { nixpkgs, purs-nix, utils, ... }:
    utils.defaultSystems
      ({ make-shell, pkgs, system }:
         let
           pn = purs-nix { inherit system; };
           inherit (pn) purs ps-pkgs;
           package = import ./package.nix pn;

           inherit
             (purs
                { inherit (package) dependencies;

                  test-dependencies =
                    with ps-pkgs;
                    [ ps-pkgs.ansi
                      ps-pkgs."assert"
                    ];

                  src = ./src;
                }
             )
             shell;
         in
         { devShell =
             make-shell
               { packages =
                   with pkgs;
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     purescript
                     (shell { inherit package; })
                   ];
               };
         }
      )
      nixpkgs;
}
