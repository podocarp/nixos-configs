{ pkgs, ... }:
{
  xdg.configFile."vifm/colors".source = ./colors;
  xdg.configFile."vifm/scripts" = {
    executable = true;
    source = ./scripts;
  };
  xdg.configFile."vifm/vifmrc".source = ./vifmrc;
  home.packages = [ pkgs.vifm pkgs.ffmpegthumbnailer ];
}
