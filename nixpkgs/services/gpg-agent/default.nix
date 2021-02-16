{ pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };
}
