{ pkgs, ... }:
{
  xsession = {
    enable = true;
    initExtra = ''
      autorandr -c
    '';
    profileExtra = ''
      xset r rate 200 30
    '';
    scriptPath = ".xsession-hm";
  };

  services.picom = {
    enable = true;
    vSync = true;
  };

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

    XTerm*faceName: xft:JuliaMono
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
  '';

  gtk = {
    # Note: this requires programs.dconf.enable = true.
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
    platformTheme = "gnome";
    style = {
      name = "Breeze";
      package = pkgs.breeze-qt5;
    };
  };
}
