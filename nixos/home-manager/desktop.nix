{ pkgs, ... }:
{
  home.stateVersion = "22.11";

  imports = [
    ./common

    ./programs/texlive
    ./programs/zathura

    ./services/dunst
    ./services/udiskie
    ./programs/xmonad
  ];

  home.packages = with pkgs; [
    dolphin

    cudatoolkit
    ddcutil
    handbrake
    inkscape
    jellyfin-media-player
    krita
    libreoffice-fresh
    tdesktop

    kicad
    openscad
    prusa-slicer
    qmk

    (runelite.overrideAttrs (oldAttrs: {
      installPhase =
        oldAttrs.installPhase
        + ''
          wrapProgram $out/bin/runelite --set GDK_SCALE 2 --set GDK_DPI_SCALE 2
        '';
    }))
  ];

  programs.man.enable = false;
}
