{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=haskell-updates";
    devenv.url = "github:cachix/devenv";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, devenv, nix-filter, ... }@inputs:
    with nix-filter.lib;
    let
      config = { allowBroken = true; };
      overlays.default = final: previous: {
        haskellPackages = with final.haskell.lib;
          previous.haskellPackages.extend (hfinal: hprevous:
            with hfinal; {
              spatial-math = callCabal2nix "spatial-math" (filter {
                root = self;
                exclude = [ "stack.yaml" (matchExt "cabal") ];
              }) { };
            });
      };
      systems = [
        "x86_64-linux"
        # "i686-linux"
        "x86_64-darwin"
        # "aarch64-linux"
        # "aarch64-darwin"
      ];
      forAllSystems = f:
        builtins.listToAttrs (map (name: {
          inherit name;
          value = f name;
        }) systems);
    in {
      inherit overlays;
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit config system;
            overlays = [ overlays.default ];
          };
        in {
          default = pkgs.haskellPackages.spatial-math;
        });
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit config system;
            overlays = [ overlays.default ];
          };
        in {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = with pkgs.haskellPackages; with pkgs; [{
              env = { name = "spatial-math"; };
              packages =
                [ (ghcWithPackages (p: with p; [ haskell-language-server spatial-math ])) ];
              scripts = {
                run-ghcid.exec = "${ghcid}/bin/ghcid -W -a -c 'cabal repl lib:spatial-math'";
                setUp.exec = ''
                  ${hpack}/bin/hpack -f package.yaml
                  ${implicit-hie}/bin/gen-hie --cabal &> hie.yaml
                '';
              };
              enterShell = "
                setUp
              ";
            }];
          };
        });
    };
}
