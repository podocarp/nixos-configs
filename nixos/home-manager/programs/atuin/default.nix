{ ... }:
{
  programs.atuin = {
    enable = true;
    enableBashIntegration = true;
    flags = [
      "--disable-up-arrow"
    ];
    settings = {
      style = "compact";
      auto_sync = false;
      exit_mode = "return-query";
      enter_accept = false;
      keymap_mode = "vim-insert";
    };
  };
}
