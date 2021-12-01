{ homeDir, ... }:

let
  path = (toString homeDir) + "/.password-store";
in
{
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = path;
    };
  };
}
