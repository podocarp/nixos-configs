{ ... }:

{
  programs.git = {
    enable = true;
    userName = "J XD";
    userEmail = "xdjiaxd@gmail.com";
    extraConfig = {
        pull.rebase = false;
        credential.helper = "cache --timeout 3600";
        init.defaultBranch = "master";
    };
  };
}
