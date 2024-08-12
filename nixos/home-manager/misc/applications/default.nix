{ pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (self: super: {
      octaveWithSym = super.octaveFull.withPackages (p: with p; [
        symbolic
      ]);
    })
  ];

  home.packages = with pkgs; [
    ### Applications
    bind # for nslookup
    entr
    ffmpeg
    file-rename
    gnumake
    highlight
    iftop
    inetutils # telnet
    iotop
    jq
    killall
    lm_sensors
    lsof
    maim
    neofetch
    nix-index
    nmap
    nomacs
    p7zip
    reptyr
    ripgrep
    syncthing
    sysstat # iostat
    tldr
    unzip
    unrar
    usbutils # for lsusb
    xterm
    zip
  ];
}
