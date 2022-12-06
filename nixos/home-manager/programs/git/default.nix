{ ... }:

{
  programs.git = {
    enable = true;
    userName = "J XD";
    userEmail = "xdjiaxd@gmail.com";
    aliases = {
      br = "branch";
      c = "commit";
      cam = "commit -a -m";
      co = "checkout";
      d = "diff";
      l = "log --graph";
      p = "push";
      s = "status";
      unstage = "reset HEAD --";
    };
    extraConfig = {
        pull.rebase = false;
        init.defaultBranch = "master";
        push.autoSetupRemote = true;
    };
  };
}
