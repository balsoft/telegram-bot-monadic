# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    telegram-bot-simple.url = "github:balsoft/telegram-bot-simple";
    telegram-bot-simple.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, telegram-bot-simple }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "telegram-bot-monadic";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self {
            # Dependency overrides go here
            telegram-bot-simple =
              haskellPackages.telegram-bot-simple_0_6.overrideAttrs
              (_: { src = telegram-bot-simple; });
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom =
            map (pkg: pkg.env) (builtins.attrValues self.packages.${system});
        };
      });
}
