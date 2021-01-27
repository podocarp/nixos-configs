{ myTerm, ... }:

{
  programs.rofi = {
    enable = true;
    borderWidth = 3;
    terminal = myTerm;
    theme = "Paper";
    font = "Liberation Mono 20";
    extraConfig = {
        modi = "combi,window";
        combi-modi = "drun,run";
    };
  };
}
