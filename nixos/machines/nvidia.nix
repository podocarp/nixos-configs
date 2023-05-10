{ pkgs, ... }:
{
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
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

  virtualisation.docker.enableNvidia = true;

  nixpkgs.config.cudaSupport = true;
}
