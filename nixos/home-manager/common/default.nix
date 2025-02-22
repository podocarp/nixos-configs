args@{ ... }:
{
  imports = [
    ./dev.nix
    ((import ./env.nix) args)
  ];

  nixpkgs.overlays = with pkgs; [
    (final: prev: {
      mytex = texlive.combine {
        inherit (texlive)
          algorithm2e
          amsmath
          beamer
          calrsfs
          cancel
          caption
          catchfile
          collection-fontsrecommended
          cleveref
          enumitem
          esint
          esint-type1
          extsizes
          fancyvrb
          float
          framed
          fvextra
          hyperref
          ifoddpage
          import
          latexmk
          lualatex-math
          libertinus-otf
          libertinus-fonts
          lineno
          mathdots
          mathtools
          minted
          multirow
          pgf
          pgfplots
          physics
          placeins
          prftree
          relsize
          rsfs
          rsfso
          scalerel
          scheme-basic
          siunitx
          stmaryrd
          tikz-cd
          unicode-math
          upquote
          wrapfig
          xkeyval
          xstring

          microtype
          threeparttable
          listings
          comment
          soul
          lastpage
          subfig
          ;
      };
    })
  ];

  programs.home-manager.enable = true;
  home.username = "pengu";
  home.homeDirectory = "/home/pengu";

  home.sessionVariables = {
    NIXPKGS_ALLOW_INSECURE = "1";
    NIXPKGS_ALLOW_UNFREE = "1";
    EDITOR = "nvim";
  };
}
