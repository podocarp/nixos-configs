{ pkgs, lib, ... }:

{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions =
      [
        { crxPath = pkgs.fetchFromGitHub {
            owner = "NeverDecaf";
            repo = "chromium-web-store";
            rev = "v1.4.3";
            sha256 = "1xxqg61g9mvxja8kp8g01dmdrd51sidfsk62jwzaazawkpdv3624";
          } + "/Chromium Web Store.crx";
          version = "1.4.3";
          id = "ocaahdebbfolfmndjeplogmgcagdmblk";
        }
      ];
  };
}
