{ pkgs, ... }:
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
    open = false;
    powerManagement.enable = true;
    nvidiaPersistenced = true;
  };

  hardware.nvidia-container-toolkit.enable = true;

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

  nixpkgs.config.cudaSupport = true;
}
