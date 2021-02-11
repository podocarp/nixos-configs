{ pkgs, ... }:

{
  programs.zathura = {
    enable = true;
    options = {
      "guioptions" = "";
      "scroll-step" = 200;
      "synctex-editor-command" = "nvr --remote +%{line} %{input}";
      "window-title-basename" = true;
      "window-title-page" = true;
    };
  };
}
