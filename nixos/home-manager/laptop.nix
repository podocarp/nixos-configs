{ pkgs, ... }:
{
  home.stateVersion = "22.11";

  imports =
    [
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
  ];

  programs.man.enable = false;

  xresources.extraConfig = "XTerm*faceSize: 11";
}
