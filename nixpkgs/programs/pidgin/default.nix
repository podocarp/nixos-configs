{ pkgs, ... }:
{
  programs.pidgin = {
    enable = true;
    plugins = with pkgs; [
      pidgin-latex
      pidgin-xmpp-receipts
      purple-xmpp-http-upload
    ];
  };
}
