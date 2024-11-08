# A common set of user environment things
{ pkgs, useX ? true, ... }:
{
  imports = [
    ../scripts

    ((import ../programs/rofi) {
      term = "xterm";
    })
    ../programs/chromium
    ../programs/mpv
    ../programs/vifm
    ../programs/vscode
    ../services/syncthing
    ../misc/keyboard
  ] ++ (if useX then [
    ../misc/xsession
  ] else [ ]);

  nixpkgs.config.allowUnfree = true;

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
    keepassxc
    killall
    lm_sensors
    lsof
    maim
    neofetch
    nix-index
    nmap
    nomacs
    p7zip
    reptyr # re-parent disowned processes
    ripgrep
    syncthing
    sysstat # iostat
    tldr
    unrar
    unzip
    usbutils # for lsusb
    xterm
    yt-dlp
    zip

    (python3.withPackages (p: with p; [
      pygments
      requests
    ]))
  ];
}
