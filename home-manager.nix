{ config, pkgs, lib, ... }: {
  options.windowManager.xmonad-ash = with lib; {
    enable = mkEnableOption "xmonad-ash";
    package = mkOption {
      type = type.pkg;
    };
  };
  config =
    let
      cfg = config.windowManager.xmonad-ash;
    in
    lib.mkIf cfg.enable {
      home.packages = [ cfg.package ];
    };
}
