{ inputs.purs-nix.url = "github:ursi/purs-nix";

  outputs = { nixpkgs, utils, purs-nix, ... }:
    utils.defaultSystems
      ({ pkgs, system }:
         with pkgs;
         let
           pn = purs-nix { inherit system; };
           inherit (pn) purs ps-pkgs;

           inherit
             (purs
                { inherit (import ./package.nix pn) dependencies;

                  test-dependencies =
                    [ ps-pkgs.ansi
                      ps-pkgs."assert"
                    ];

                  src = ./src;
                }
             )
             shell;
         in
         { devShell =
             with pkgs;
             mkShell
               { buildInputs =
                   [ nodejs
                     nodePackages.bower
                     nodePackages.pulp
                     purescript
                     (shell {})
                   ];
               };
         }
      )
      nixpkgs;
}
