{ myTerm, ... }:

{
  xdg.configFile."vifm/colors".source = ./colors;
  xdg.configFile."vifm/scripts" = {
    executable = true;
    source = ./scripts;
  };
  xdg.configFile."vifm/vifmrc".text = builtins.readFile ./vifmrc + ''
    filetype *
            \ {Open in vim}
            \ ${myTerm} -e vim %c &
  '';
}
