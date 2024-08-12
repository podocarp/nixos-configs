{ pkgs, ... }:
{
  # needed for some funny xdg business, not sure why
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "*";
  };

  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5 = {
      addons = with pkgs; [
        fcitx5-chinese-addons
        fcitx5-table-extra
      ];
      settings = {
        inputMethod = {
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = "us";
            "DefaultIM" = "keyboard-us";
          };
          "Groups/0/Items/0" = {
            Name = "keyboard-us";
            Layout = "";
          };
          "Groups/0/Items/1" = {
            Name = "pinyin";
            Layout = "";
          };
          "GroupOrder" = {
            "0" = "Default";
          };
        };
      };
    };
  };
}
