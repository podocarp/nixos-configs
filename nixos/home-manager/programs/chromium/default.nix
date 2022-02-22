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
            sha256 = "sha256-xKjtW3O0TMtzxMeekssD/4QTd7pe3c1H5Dep31g+OBY=";
          } + "/Chromium Web Store.crx";
          version = "1.4.3";
          id = "ocaahdebbfolfmndjeplogmgcagdmblk";
        }
      ];
  };
}
