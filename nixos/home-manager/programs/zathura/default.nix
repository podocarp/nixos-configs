{ pkgs, ... }:
{
  programs.zathura = {
    enable = true;
    options = {
      "guioptions" = "";
      "scroll-step" = 200;
      "window-title-basename" = true;
      "window-title-page" = true;
    };
  };
}
