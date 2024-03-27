{ pkgs, ... }:
{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    enableBashIntegration = true;
    pinentryPackage = pkgs.pinentry-tty;
  };
}
