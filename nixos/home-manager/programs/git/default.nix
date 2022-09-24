{ ... }:

{
  programs.git = {
    enable = true;
    userName = "J XD";
    userEmail = "xdjiaxd@gmail.com";
    extraConfig = {
        pull.rebase = false;
        init.defaultBranch = "master";
        push.autoSetupRemote = true;
    };
  };
}
