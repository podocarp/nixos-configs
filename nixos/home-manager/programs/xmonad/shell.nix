with import <nixpkgs> {};

pkgs.mkShell {
  nativeBuildInputs = [
    (haskellPackages.ghcWithPackages(hp: [
      hp.xmonad
      hp.xmonad-contrib
    ]))
  ];
}
