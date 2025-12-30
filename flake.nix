{
  description = "randstr (Racket) - nixpkgs-friendly flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        randstr = pkgs.callPackage ./nix/package.nix { src = self; };
      in
      {
        packages.default = randstr;

        apps.default = flake-utils.lib.mkApp {
          drv = randstr;
          exePath = "bin/randstr";
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ randstr ];
          packages = with pkgs; [ just git ];

          shellHook = ''
            echo "Entering randstr dev shell"
            echo "- Racket: $(command -v racket >/dev/null 2>&1 && racket --version || echo missing)"
            echo "- Try: nix build / nix run / just test"
          '';
        };
      });
}
