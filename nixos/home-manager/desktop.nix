{ pkgs, ... }:
{
  home.stateVersion = "22.11";

  imports =
    [
      ./common

      ./programs/texlive
      ./programs/zathura
    ];

  home.packages = with pkgs; [
    cudatoolkit
    ddcutil
    handbrake
    inkscape
    jellyfin-media-player
    krita
    libreoffice-fresh
    runelite
    tdesktop

    kicad
    openscad
    prusa-slicer

    godot_4
  ];

  programs.man.enable = false;
}
