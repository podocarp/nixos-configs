{ pkgs, ... }:

let
  mystyle.pkgs = [ (pkgs.callPackage (import ./mystyle/mystyle.nix) {}) ];
in
{
  programs.texlive = {
    enable = true;
    extraPackages = texpkgs : {
      inherit (texpkgs)
        scheme-small
        tikz-cd
        latexmk
        cleveref;
      inherit mystyle;
    };
  };
}
