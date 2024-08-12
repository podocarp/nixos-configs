{ pkgs, ... }:
{
  xsession = {
    enable = true;
    initExtra = ''
      # kwriteconfig5 --file startkderc --group General --key systemdBoot false
      # export KDEWM=xmonad
    '';
    profileExtra = ''
      xset r rate 200 30
    '';
    scriptPath = ".xsession-hm";
  };

  home.packages = with pkgs;
    [
      xclip
      xdotool # for synctex
      xorg.xev
      xorg.xkill
      xorg.xprop
    ];

  xresources.extraConfig =
    ''
      ! PaperColor Theme
      *.foreground: #4D4D4C
      *.background: #FFFFFF
      ! black
      *.color0: #EDEDED
      *.color8: #969694
      ! red
      *.color1: #D7005F
      *.color9: #D7005F
      ! green
      *.color2: #718C00
      *.color10: #718C00
      ! yellow / orange
      *.color3: #D75F00
      *.color11: #D75F00
      ! blue
      *.color4: #4271AE
      *.color12: #4271AE
      ! magenta
      *.color5: #8959A8
      *.color13: #8959A8
      ! cyan
      *.color6: #3E999F
      *.color14: #3E999F
      ! white
      *.color7: #F5F5F5
      *.color15: #2D2D2C

      Xcursor.size: 40

      XTerm*faceName: xft:DroidSansM Nerd Font Mono
      XTerm*faceSize: 10
      XTerm*forceBoxChars: false
      XTerm.vt100.translations: #override \n\
        Ctrl <Key> minus: smaller-vt-font() \n\
        Ctrl <Key> plus: larger-vt-font() \n\
        Ctrl <Key> 0: set-vt-font(d) \n\
        Ctrl Shift <Key> C: copy-selection(CLIPBOARD) \n\
        Ctrl Shift <Key> V: insert-selection(CLIPBOARD)
      XTerm*backarrowKey: false
      XTerm*decTerminalID: vt340
      XTerm*metaSendsEscape: true
      XTerm*numColorRegisters: 256
      XTerm*pointerColor: black
      XTerm*pointerColorBackground: black
      XTerm*pointerShape: left_ptr
      XTerm*ptyInitialErase: true
      XTerm*termName: st-256color
      XTerm*trimSelection: true
      XTerm*ttyModes: erase ^?
      XTerm*allowWindowOps: true
      XTerm*selectToClipboard: true
      XTerm*maxStringParse: 100000000
      XTerm*maxGraphicSize: 3000x3000
    '';

  gtk = {
    enable = true;
    iconTheme = {
      name = "Breeze";
      package = pkgs.breeze-icons;
    };
    theme = {
      name = "Breeze";
      package = pkgs.breeze-gtk;
    };
  };
  qt = {
    enable = true;
    platformTheme.name = "adwaita";
    style = {
      name = "Breeze";
      package = pkgs.breeze-qt5;
    };
  };

  services.picom = {
    enable = false;
    activeOpacity = 1.0;
    inactiveOpacity = 1.0;
    fade = false;
    shadow = false;
    vSync = true;
  };
}
