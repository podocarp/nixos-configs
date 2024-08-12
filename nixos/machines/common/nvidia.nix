{ pkgs, lib, ... }:
{
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      libvdpau-va-gl
      vaapiVdpau
    ];
  };

  hardware.nvidia = {
    powerManagement.enable = true;
    nvidiaPersistenced = true;
  };

  environment.variables = {
    "LIBVA_DRIVER_NAME" = "vdpau";
    "VDPAU_DRIVER" = "nvidia";
  };

  environment.systemPackages = with pkgs; [
    binutils
    cudatoolkit
    freeglut
    libGL
    libGLU
    linuxPackages.nvidia_x11
    stdenv.cc
  ];

  virtualisation.docker.enableNvidia = true;

  nixpkgs.config.cudaSupport = true;
}
