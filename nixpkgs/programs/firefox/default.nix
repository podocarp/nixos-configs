{ pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    enableAdobeFlash = false;
    enableGnomeExtensions = false;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      clearurls
      decentraleyes
      ecosia
      tree-style-tab
      ublock-origin
      unpaywall
      vimium
    ];
    profiles = {
      custom = {
        id = 0;
        userChrome = ''
          #TabsToolbar { visibility: collapse !important; }
        '';
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.ctrlTab.recentlyUsedOrder" = false;
          "browser.newtabpage.enabled" = false;
          "browser.startup.homepage" = "https://en.wikipedia.org/wiki/Special:Random";
          "browser.urlbar.update2" = false;
          "general.autoScroll" = true;
          "general.smoothScroll" = false;
        };
        extraConfig = builtins.readFile ./user.js;
      };
    };
  };
}
