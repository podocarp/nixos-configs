{ ... }:

{
  programs.git = {
    enable = true;
    userName = "J XD";
    userEmail = "xdjiaxd@gmail.com";
    aliases = {
      co = "checkout";
      c = "commit";
      cam = "commit -a -m";
      br = "branch";
      s = "status";
      l = "log --graph";
      unstage = "reset HEAD --";
    };
    extraConfig = {
        pull.rebase = false;
        init.defaultBranch = "master";
        push.autoSetupRemote = true;
    };
  };
}
