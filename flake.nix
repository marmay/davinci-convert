{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: _prev: {
          # This overlay adds our project to pkgs
          davinci-convert-project =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc966";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              shell.nativeBuildInputs = with pkgs; [
                ffmpeg
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.davinci-convert-project.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:davinci-convert:exe:davinci-convert`
        # crossPlatforms = p: [p.javascript];
      };
    in flake // {
      # Built by `nix build .`
      packages.default = flake.packages."davinci-convert:exe:davinci-convert";
    });
}
