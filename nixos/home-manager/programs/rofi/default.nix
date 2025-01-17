{ term, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = term;
    theme = "Paper";
    font = "Liberation Mono 20";
    extraConfig = {
      modi = "combi,window";
      combi-modi = "drun,run";
      monitor = "-1";
    };
    pass = {
      enable = true;
      stores = [ "~/.password-store" ];
      extraConfig = ''
        EDITOR='${term} -e vi'
        type_pass="Alt+1"
        type_user="Alt+2"
        autotype="Alt+3"
        open_url="Alt+4"
        copy_name="Alt+u"
        copy_url="Alt+l"
        copy_pass="Alt+p"
        show="Alt+o"
        copy_entry="Alt+2"
        type_entry="Alt+1"
        copy_menu="Alt+c"
        action_menu="Alt+a"
        type_menu="Alt+t"
        help="Alt+h"
        help_color="#000000"
        switch="Alt+x"
        insert_pass="Alt+n"
      '';
    };
  };
}
