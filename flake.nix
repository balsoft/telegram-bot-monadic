# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    telegram-bot-simple.url = "github:fizruk/telegram-bot-simple";
    telegram-bot-simple.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, telegram-bot-simple }:
    let packageName = "telegram-bot-monadic";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
      in {
        legacyPackages = haskellPackages.extend self.overlays.default;

        packages.${packageName} = self.legacyPackages.${system}.${packageName};

        packages.default = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom =
            map (pkg: pkg.env) (builtins.attrValues self.packages.${system});
        };
      }) // {
        overlays.default = final: prev: {
          telegram-bot-simple = prev.telegram-bot-simple_0_6.overrideAttrs
            (_: { src = telegram-bot-simple; });
          ${packageName} = final.callCabal2nix packageName self { };
        };
      };
}
