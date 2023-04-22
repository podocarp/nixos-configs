{ config, pkgs, lib, ... }:

{
  imports = [
    ./common.nix
    ../misc/xserver.nix
  ];

  boot = {
    loader.grub = {
      useOSProber = true;
      efiSupport = true;
      device = "nodev";
    };
    loader.efi.canTouchEfiVariables = true;
    kernelModules = [ "kvm-amd" ];
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/06fa2c05-663a-4b3b-9f3f-7d1627190338";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/1618-9A73";
      fsType = "vfat";
    };

  fileSystems."/shared" =
    {
      device = "/dev/disk/by-uuid/D2A2317CA23165DF";
      fsType = "ntfs";
      options = [ "rw" "uid=1000" ];
    };

  fileSystems."/network/smb" =
    {
      device = "//obsidian/public";
      fsType = "cifs";
      options = [
        "_netdev"
        "user"
        "uid=1000"
        "gid=100"
        "forceuid"
        "forcegid"
        "x-systemd.idle-timeout=10min"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
        "username=fileshare"
        "password=fileshare"
        "cache=loose"
      ];
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/d9fbd022-c7d0-4ee3-95a1-bbab0e3585f0"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "schedutil";

  sops = {
    defaultSopsFile = ../secrets/secrets.yaml;
    gnupg.sshKeyPaths = [ ];
    age.keyFile = "/var/lib/sops/age/keys.txt";
  };


  networking.hostName = "desktop"; # Define your hostname.

  services.xserver.videoDrivers = [ "nvidia" ];

  # Some hardware acceleration things.
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      nvidia-vaapi-driver
      vaapiVdpau
    ];
  };

  hardware.nvidia = {
    powerManagement.enable = true;
  };

  environment.variables = {
    "LIBVA_DRIVER_NAME" = "vdpau";
    "VDPAU_DRIVER" = "nvidia";
  };

  systemd.services.fix_acpi_wakeup = {
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    description = "Prevents USB from waking the system up so the system can sleep properly.";
    script = ''
      ${pkgs.bash}/bin/bash -c 'echo PTXH > /proc/acpi/wakeup; echo XHC0 > /proc/acpi/wakeup;'
    '';
    wantedBy = [ "multi-user.target" ];
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "pengu" ];

  virtualisation.docker.enable = true;
  virtualisation.oci-containers.backend = "docker";

  home-manager.users.pengu = import ../home-manager/desktop.nix;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  nixpkgs.config.cudaSupport = true;
}
