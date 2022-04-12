{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    useGlamor = true;

    # This starts ~/.xsession, which allows home-manager to control some things.
    displayManager.session = [
      {
        name = "xsession";
        start = "${pkgs.runtimeShell} $HOME/.xsession-hm & waitPID=$!";
        manage = "window";
      }
    ];
  };
}
