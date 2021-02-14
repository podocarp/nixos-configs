{ pkgs, ... }:

{
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "~/.password-store";
    };
  };
}
