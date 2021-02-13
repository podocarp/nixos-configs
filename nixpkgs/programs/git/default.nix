{ ... }:

{
  programs.git = {
    enable = true;
    userName = "J XD";
    userEmail = "xdjiaxd@gmail.com";
    extraConfig = {
        pull.rebase = false;
        credential.helper = "cache";
        init.defaultBranch = "master";
    };
  };
}
