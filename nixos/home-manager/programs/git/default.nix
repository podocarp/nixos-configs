{ ... }:

{
  programs.git = {
    enable = true;
    userName = "J XD";
    userEmail = "xdjiaxd@gmail.com";
    aliases = {
      a = "add";
      aa = "add -A";
      b = "branch";
      c = "commit";
      ca = "commit -a";
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
