{ ... }:

{
  programs.texlive = {
    enable = true;
    extraPackages = texpkgs : {
      inherit (texpkgs)
        algorithm2e
        amsmath
        beamer
        biber
        biblatex
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
        physics
        prftree
        relsize
        rsfs
        rsfso
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
