{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";
    flake-utils.url  = "github:numtide/flake-utils";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in {
        devShells.default = pkgs.mkShell {
          # You now have access to the standard PureScript toolchain in pkgs
          buildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy-bin.purs-tidy-0_10_0
            pkgs.purs-backend-es
            pkgs.nodejs-18_x
          ];
        };
      }
    );
}
