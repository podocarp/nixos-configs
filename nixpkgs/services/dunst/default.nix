{ pkgs, ... }:

{
  services.dunst = {
    enable = true;
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper";
    settings = {
      global = {
        font = "DejaVu Sans";
        markup = true;
        format = "<b>%s</b>\n%b%p";
        sort = false;
        indicate_hidden = true;
        alignment = "center";
        word_wrap = true;
        stack_duplicates = true;
        hide_duplicates_count = false;
        geometry = "300x50-15+49";
        shrink = false;
        transparency = 0;
        idle_threshold = 0;
        follow = "keyboard";
        history_length = 10;
        frame_width = 1;
        frame_color = "#8ECC7C";
        browser = "${pkgs.firefox}/bin/firefox";
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+mod1+space";
        history = "ctrl+grave";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        frame_color = "#66AB76";
        foreground = "#AAFFAA";
        background = "#336644";
        timeout = 5;
      };
      urgency_normal = {
        frame_color = "#FFFFFF";
        foreground = "#FFFFFF";
        background = "#333333";
        timeout = 5;
      };
      urgency_critical = {
        frame_color = "#B7472A";
        foreground = "#B7472A";
        background = "#191311";
        timeout = 10;
      };
    };
  };
}
