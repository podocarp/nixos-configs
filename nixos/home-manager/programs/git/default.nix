{ ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "podocarp";
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
      stash-all = "stash --include-untracked";
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
        keepBackup = false;
      };
      color = {
        ui = true;
        diff = {
          meta = "black bold";
          commit = "green";
          frag = "magenta";
          func = "cyan";
          old = "red";
          new = "green";
          whitespace = "red reverse";
        };
        diff-highlight = {
          oldNormal = "red";
          oldHighlight = "red bold 52";
          newNormal = "green";
          newHighlight = "green bold 22";
        };
        branch = {
          current = "yellow reverse";
          local = "yellow";
          remote = "green";
        };
      };
    };

    diff-so-fancy = {
      enable = true;
    };
  };
}
