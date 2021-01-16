{ ... }:

{
  services.random-background = {
    enable = true;
    enableXinerama = true;
    display = "fill";
    # systemd path format. %h is home dir.
    imageDirectory = "%h/Images/wallpapers";
    # only set once on login.
    interval = null;
  };
}
