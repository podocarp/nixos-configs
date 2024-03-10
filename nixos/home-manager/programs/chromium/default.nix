{ pkgs, lib, ... }:

{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions =
      [
        {
          crxPath = pkgs.fetchFromGitHub
            {
              owner = "NeverDecaf";
              repo = "chromium-web-store";
              rev = "v1.5.4";
              sha256 = "sha256-Rr0KVs6Ztqz04CpQSDThn/hi6VZdVZsztPSALUY/fnE=";
            } + "/Chromium Web Store.crx";
          version = "1.4.3";
          id = "ocaahdebbfolfmndjeplogmgcagdmblk";
        }
      ];
  };
}
