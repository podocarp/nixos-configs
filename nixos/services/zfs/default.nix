{ ... }:
{
  services.zfs = {
    autoSnapshot = {
      enable = true;
      frequent = 0;
      hourly = 0;
      daily = 6;
      weekly = 4;
      monthly = 0;
    };
    autoScrub = {
      enable = true;
      pools = [ "tank" ];
      interval = "monthly";
    };
    trim = {
      enable = true;
      interval = "weekly";
    };
  };
}
