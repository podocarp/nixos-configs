{ ... }:

{
  services.random-background = {
    enable = true;
    enableXinerama = true;
    display = "fill";
    # systemd path format. %h is home dir.
    imageDirectory = "%h/Pictures/wallpapers";
    interval = "1h";
  };
}
