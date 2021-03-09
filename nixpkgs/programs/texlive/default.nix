{ ... }:

{
  programs.texlive = {
    enable = true;
    extraPackages = texpkgs : {
      inherit (texpkgs)
        algorithm2e
        amsmath
        biber
        biblatex
        calrsfs
        cancel
        caption
        catchfile
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
        mathtools
        microtype
        minted
        multirow
        pgf
        physics
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
