{ pkgs, ... }:
{
  programs.zathura = {
    enable = true;
    options = {
      "guioptions" = "";
      "scroll-step" = 200;
      "synctex" = true;
      "synctex-editor-command" = "nvr --remote-silent +%{line} %{input}";
    };
  };
}
