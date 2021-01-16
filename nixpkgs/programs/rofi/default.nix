{ myTerm, ... }:

{
  programs.rofi = {
    enable = true;
    borderWidth = 3;
    terminal = myTerm;
    theme = "Paper";
  };
}
