{ ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "J XD";
    userEmail = "xdjiaxd@gmail.com";
    aliases = {
      a = "add";
      aa = "add -A";
      b = "branch";
      c = "commit";
      ca = "commit -a";
      cm = "commit -m";
      cam = "commit -a -m";
      co = "checkout";
      d = "diff";
      l = "log --graph --author-date-order --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(green)%an%C(reset)%C(bold yellow)%d%C(reset) - %s'";
      p = "push";
      s = "status";
      unstage = "reset HEAD --";
    };
    extraConfig = {
      pull.rebase = false;
      init.defaultBranch = "master";
      push.autoSetupRemote = true;
      mergetool = {
        tool = "vimdiff";
        prompt = false;
        conflictstyle = "diff3";
      };
    };
  };
}
