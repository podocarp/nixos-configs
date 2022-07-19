{ pkgs, lib, ... }:

let
  equilux-firefox = pkgs.fetchFromGitHub {
    owner = "cj-sv";
    repo = "equilux-firefox";
    rev = "2c6f0ad";
    sha256 = "0ryxvi4iga6q2czn52h8gg3c67asz6vkzkc1252m090fjgd98qyd";
  };
in
{
  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      clearurls
      decentraleyes
      ecosia
      privacy-badger
      ublock-origin
      unpaywall
      vimium
    ];
    profiles = {
      custom = {
        id = 0;
        userChrome = builtins.readFile "${equilux-firefox}/userChrome.css" + ''
          // #TabsToolbar { visibility: collapse !important; }
          #contentAreaContextMenu{ margin: 20px 0 0 20px }
        '';
        userContent = builtins.readFile "${equilux-firefox}/userContent.css";
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.ctrlTab.recentlyUsedOrder" = false;
          "browser.warnOnQuit" = false;
          "browser.newtabpage.enabled" = false;
          "browser.startup.homepage" = "https://en.wikipedia.org/wiki/Special:Random";
          "browser.urlbar.update2" = false;
          "dom.webnotifications.enabled" = false;
          "findbar.highlightAll" = true;
          "general.autoScroll" = true;
          "general.smoothScroll" = false;
          "media.ffmpeg.vaapi.enabled" = true;
          "media.ffvpx.enabled" = false;
        };
        extraConfig = builtins.readFile ./user.js;
      };
    };
  };
}
