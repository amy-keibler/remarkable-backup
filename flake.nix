{
  description = "Back up the Remarkable 2 over SSH and install custom templates";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          remarkable-backup =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8105";
              shell.tools = {
                cabal = {};
                ghcid = {};
                haskell-language-server = {};
                hlint = {};
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.remarkable-backup.flake {
        crossPlatforms = p: [];
      };
    in flake // {
      defaultPackage = flake.packages."remarkable-backup:exe:remarkable-backup";
    });
}
