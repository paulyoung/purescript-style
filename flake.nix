{
  inputs = {
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    nixpkgs.url = "github:NixOS/nixpkgs/21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    easy-purescript-nix,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };

        easy-ps = import easy-purescript-nix { inherit pkgs; };
        spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
      in
        rec {
          # `nix build`
          packages.purescript-style = pkgs.stdenv.mkDerivation {
            name = "purescript-style";
            buildInputs = [
              spagoPkgs.installSpagoStyle
              spagoPkgs.buildSpagoStyle
            ];
            nativeBuildInputs = [
              easy-ps.purs
              easy-ps.spago
            ];
            src = ./.;
            unpackPhase = ''
              cp $src/spago.dhall .
              cp $src/packages.dhall .
              cp -R $src/src .
              install-spago-style
            '';
            buildPhase = ''
              build-spago-style "./src/**/*.purs" --codegen corefn
            '';
            installPhase = ''
              mkdir $out
              mv output $out/
            '';
          };

          defaultPackage = packages.purescript-style;

          # `nix develop`
          devShell = pkgs.mkShell {
            buildInputs = [
              easy-ps.psa
              easy-ps.purs
              easy-ps.purs-tidy
              easy-ps.purescript-language-server
              easy-ps.spago
              easy-ps.spago2nix
              pkgs.nodejs
            ];
          };
        }
    );
}
