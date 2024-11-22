{ pkgs, ... }:
{
  home.stateVersion = "22.11";

  imports = [
    ./common

    ./programs/autorandr
    ./programs/texlive
    ./programs/zathura

    ./services/dunst
    ./programs/xmonad
  ];

  home.packages = with pkgs; [
    arandr
    tdesktop
    brightnessctl
    runelite
  ];

  xresources.extraConfig = "XTerm*faceSize: 11";
}
