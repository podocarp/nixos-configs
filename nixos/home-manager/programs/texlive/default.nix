{ ... }:

{
  programs.texlive = {
    enable = true;
    extraPackages = texpkgs: {
      inherit (texpkgs)
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
  };
}
