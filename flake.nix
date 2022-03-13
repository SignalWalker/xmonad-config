{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    git-ignore-nix.url = github:hercules-ci/gitignore.nix/master;
    xmonad = github:xmonad/xmonad;
    xmonad-contrib = {
      url = path:./xmonad-contrib;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, git-ignore-nix, xmonad, xmonad-contrib }:
    let
      pname = "xmonad-ash";
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: hsuper: {
              ${pname} =
                hself.callCabal2nix pname (git-ignore-nix.lib.gitignoreSource ./.) { };
            });
        });
      };
      overlays = xmonad.overlays ++ xmonad-contrib.overlays ++ [ overlay ];
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
          modifyDevShell =
            if builtins.pathExists ./develop.nix
            then import ./develop.nix
            else _: x: x;
        in
        rec {
          devShell = pkgs.haskellPackages.shellFor (modifyDevShell pkgs {
            packages = p: [ p.${pname} ];
            nativeBuildInputs = [ pkgs.cabal-install ];
          });
          defaultPackage = pkgs.haskellPackages.${pname};
        }) // { inherit overlay overlays; };
}
