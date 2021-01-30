# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, libs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ''${builtins.fetchGit {
          url = "https://github.com/NixOS/nixos-hardware.git";
	} }/lenovo/thinkpad/x1-extreme''
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "nodev";
    efiSupport = true;
    efiInstallAsRemovable = true;
    useOSProber = true;
  };

  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1
    options thinkpad_acpi fan_control
  '';

  boot.blacklistedKernelModules = [ "snd_hda_codec_hdmi" ];

  boot.kernel.sysctl = {
    "kernel.nmi_watchdog" = 0;
    "vm.dirty_writeback_centisecs" = 6000;
    "vm.laptop_mode" = 5;
  };

  # Set your time zone.
  time.timeZone = "Asia/Singapore";

  networking = {
    hostName = "pebble"; # Define your hostname.
    wireless = {
      enable = true;  # Enables wireless support via wpa_supplicant.
      networks = {
        "polypeng" = {
          pskRaw = "e8a41b5c49b4690579db770e28cf6903fe69e1b624a9bab8bf06ceace0001633";
        };
      };
    };
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = true;
  #networking.interfaces.enp0s25.useDHCP = true;
  #networking.interfaces.wls1.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  services.xserver = {
    enable = true;
    xkbOptions = "caps:escape";

    libinput = {
      enable = true;
      touchpad.scrollButton = 2;
    };

    displayManager.session = [
      {
        name = "xsession";
        start = "${pkgs.runtimeShell} $HOME/.xsession & waitPID=$!";
        manage = "window";
      }
    ];

    videoDrivers = [ "nvidia" "modesetting" ];
    useGlamor = true;
  };

  hardware.nvidia.prime = {
    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
  };

  hardware.opengl = {
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  services.thinkfan.enable = true;
  services.thinkfan.levels = ''
    (0,     0,      55)
    (1,     48,     60)
    (2,     50,     61)
    (3,     52,     63)
    (6,     56,     65)
    (7,     60,     85)
    (127,   80,     32767)
  '';

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="input", ATTR{name}=="TPPS/2 IBM TrackPoint", ATTR{device/press_to_select}="1"
    ACTION=="add", SUBSYSTEM=="scsi_host", KERNEL=="host*", ATTR{link_power_management_policy}="med_power_with_dipm"
  '';

  services.undervolt = {
    enable = true;
    coreOffset = -150;
    analogioOffset = -100;
    uncoreOffset = -30;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.pengu = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    pciutils
    vim
    wget
  ];

  i18n.inputMethod.enabled = "fcitx";
  i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';

  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
