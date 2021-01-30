{ pkgs, ... }:

{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
      { id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp"; } # privacy badger
      { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # dark reader
      { id = "fngmhnnpilhplaeedifhccceomclgfbg"; } # edit this cookie
      { id = "djflhoibgkdhkhhcedjiklpkjnoahfmg"; } # user agent switcher
      { id = "fbfecjjfhcgpocehenopdofhkdjfpcgl"; } # fready
    ];
  };
}
