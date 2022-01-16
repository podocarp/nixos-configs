{ pkgs, ... }:
{
  i18n.inputMethod.enabled = "fcitx";
  i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];
  xsession.initExtra = ''
      export XMODIFIERS = "@im=fcitx"
      export XMODIFIER = "@im=fcitx"
      export GTK_IM_MODULE = "@im=fcitx"
      export QT_IM_MODULE = "@im=fcitx"
  '';
}
