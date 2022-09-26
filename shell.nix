with import <nixpkgs> {};

pkgs.mkShell {
  nativeBuildInputs = [
    (haskellPackages.ghcWithPackages (hp: [
      hp.xmonad
      hp.xmonad-contrib
      hp.regex-posix
    ]))
    ranger
    nixops_unstable
    sops
  ];
}
