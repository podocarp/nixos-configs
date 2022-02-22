{ ... }:

{
  programs.texlive = {
    enable = true;
    extraPackages = texpkgs : {
      inherit (texpkgs)
        algorithm2e
        amsmath
        beamer
        # biber
        # biblatex
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
        lineno
        mathdots
        mathtools
        microtype
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
        upquote
        wrapfig
        xkeyval
        xstring
        ;
    };
  };
}
