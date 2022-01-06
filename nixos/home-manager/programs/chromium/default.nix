{ pkgs, lib, ... }:

{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions =
      let
        getExt =
          { id, sha256, version }:
          {
            inherit id;
            inherit version;
            crxPath = builtins.fetchurl {
              inherit sha256;
              name = "${id}.crx";
              url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${lib.versions.major pkgs.ungoogled-chromium.version}&x=id%3D${id}%26installsource%3Dondemand%26uc";
            };
          };
      in
    [
      # clearurls
      (getExt {
        id = "lckanjgmijmafbedllaakclkaicjfmnk";
        sha256 = "12vijbyhr137d1r57x0gj8fwfmyz400x8x853xdhzd4sn99w7fza";
        version = "1.22.0";
      })
      # privacy badger
      (getExt {
        id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
        sha256 = "0wgr2q7x01naqj1fh6wmam36x89dig7qdrr4xy043k6c0id7j776";
        version = "2021.11.23.1";
      })
      # ublock origin
      (getExt {
        id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        sha256 = "0jxkiq53l5q9c12gi630zmy367hh81xibr2wx1pw54i8n9hn8yqr";
        version = "1.40.4";
      })
      # vimium
      (getExt {
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
        sha256 = "097axwrhn8g26kp25w86x71khaqcw3nb0ras9ndwqvdw3bpgkcd8";
        version = "1.67";
      })
      { crxPath = pkgs.fetchFromGitHub {
          owner = "NeverDecaf";
          repo = "chromium-web-store";
          rev = "v1.4.0";
          sha256 = "1xxqg61g9mvxja8kp8g01dmdrd51sidfsk62jwzaazawkpdv3624";
        } + "/Chromium Web Store.crx";
        version = "1.4.0";
        id = "ocaahdebbfolfmndjeplogmgcagdmblk";
      }
    ];
  };
}
