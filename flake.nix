{
  description = "YAML parser in Haskell";

  inputs = {
   nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: with self; {
          haskell-yaml = haskellPackages.callCabal2nix "haskell-yaml" ./. { };
        };

        overlays = [ overlay ];

        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      {
        defaultPackage = pkgs.haskell-yaml;
        packages = {
          inherit (pkgs) haskell-yaml;
        };

        defaultApp = pkgs.haskell-yaml;
        apps = {
          inherit (pkgs) haskell-yaml;
        };

        devShell = with pkgs; mkShell {
          inputs = [
            haskell-yaml
          ];

          buildInputs = [
            haskell-language-server
            cabal-install
            ghc
          ];
        };
      }
    );
}
