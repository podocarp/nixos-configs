{ pkgs, lib, ... }:

{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions =
      let
        createChromiumExtensionFor = browserVersion: { id, sha256, version }:
          {
            inherit id;
            crxPath = builtins.fetchurl {
              url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
              name = "${id}.crx";
              inherit sha256;
            };
            inherit version;
          };
        createChromiumExtension = createChromiumExtensionFor (lib.versions.major pkgs.ungoogled-chromium.version);
      in
      [
        {
          crxPath = pkgs.fetchFromGitHub
            {
              owner = "NeverDecaf";
              repo = "chromium-web-store";
              rev = "v1.4.3";
              sha256 = "sha256-xKjtW3O0TMtzxMeekssD/4QTd7pe3c1H5Dep31g+OBY=";
            } + "/Chromium Web Store.crx";
          version = "1.4.3";
          id = "ocaahdebbfolfmndjeplogmgcagdmblk";
        }
        (
          # keepassx
          createChromiumExtension {
            id = "oboonakemofpalcgghocfoadofidjkkk";
            sha256 = "sha256:0mm04zhgsiddnl5mkagcqbg2cysy2rhhbpky24xa4bhy8yb3jl20";
            version = "1.8.5.1";
          }
        )
      ];
  };
}
