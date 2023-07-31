{ pkgs }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hp: [
      hp.xmonad
      hp.xmonad-contrib
      hp.regex-posix
    ]))
    sops
    prometheus.cli
    wireguard-tools
  ];
}
