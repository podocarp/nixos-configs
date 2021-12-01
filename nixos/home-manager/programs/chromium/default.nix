{ pkgs, lib, ... }:

{
  programs.chromium = {
    enable = true;
    package = pkgs.chromium;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"; } # privacy badger
      { id = "djflhoibgkdhkhhcedjiklpkjnoahfmg"; } # user agent switcher
      { crxPath = pkgs.fetchFromGitHub {
          owner = "NeverDecaf";
          repo = "chromium-web-store";
          rev = "master";
          sha256 = "1xxqg61g9mvxja8kp8g01dmdrd51sidfsk62jwzaazawkpdv3624";
        } + "/Chromium Web Store.crx";
        version = "1.3.0";
        id = "ocaahdebbfolfmndjeplogmgcagdmblk";
      }
    ];
  };
}
