{ ... }:

{
  # Some nice pics I host.
  home.file.wallpapers = {
    source = builtins.fetchTarball {
      url = "https://jiaxiaodong.com/img/wallpapers/Wallpapers.tar";
      sha256 = "10992gd3z77r3jaz5dnk0w3ql1nys9sz6swr1n8irxrxw8iqqr9g";
    };
    target = "Images/wallpapers";
  };
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
