{ pkgs, ... }:

{
  xdg.configFile."vifm/colors".source = ./colors;
  xdg.configFile."vifm/scripts".source = ./scripts;
  xdg.configFile."vifm/vifmrc".source = ./vifmrc;
}
