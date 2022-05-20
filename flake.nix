{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    xmonad = {
      url = github:xmonad/xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = github:signalwalker/xmonad-contrib/ash;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };
  outputs = { self, nixpkgs, xmonad, xmonad-contrib }:
    let
      std = nixpkgs.lib;
      derivations = {
        "x86_64-linux" = {
          haskellPackages = final: prev: prev.haskellPackages.override {
            overrides = hfinal: hprev: {
              # xmonad = (xmonad.overlay hfinal hprev).haskellPackages.xmonad;
              # xmonad-contrib = (xmonad-contrib.overlay hfinal hprev).haskellPackages.xmonad-contrib;
              process = hprev.callHackageDirect
                {
                  pkg = "process";
                  ver = "1.6.14.0";
                  sha256 = "REpymPsVz7EMCWbSYzqGb+wr0IpcvCzcdjio/a0gFzU=";
                }
                { };
              directory = hprev.callHackageDirect
                {
                  pkg = "directory";
                  ver = "1.3.7.0";
                  sha256 = "GoqLXqGGYHAMAQrt3B8YOI2t9EhbCjobxA9imI0N84E=";
                }
                { };
              xmonad-ash = hprev.callCabal2nix "xmonad-ash" (./.) { };
            };
          };
        };
      };
      mapDrvs = fn: std.mapAttrs fn derivations;
    in
    {
      overlays = mapDrvs
        (system: pkgs:
          final: prev: ((std.mapAttrs (name: drv: drv final prev) pkgs))
        );
      packages = mapDrvs (system: drvs:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.${system} ] ++ xmonad-contrib.overlays; };
        in
        (std.mapAttrs (name: drv: pkgs.${name}) drvs) // {
          default = pkgs.haskellPackages.xmonad-ash;
        });
      homeManagerModules = { xmonad-ash = import ./home-manager.nix; };
    };
}
