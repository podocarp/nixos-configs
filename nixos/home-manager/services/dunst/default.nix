{ pkgs, config, homeDir, ... }:

let
  # This should match the
  soundpath = ".config/dunst";
in
{
  # Sound is taken from KDE repo.
  home.file."${soundpath}/boop.ogg".source = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/KDE/oxygen/master/sounds/Oxygen-Sys-App-Message.ogg";
    sha256 = "0lsc7fn0jzf1avkx3k6dzxl1zcxvdiz5xs7c0f4d73lzw83r1sb8";
  };
  home.file."${soundpath}/boop.sh" = {
    text = "${pkgs.mpv}/bin/mpv ${homeDir}/${soundpath}/boop.ogg";
    executable = true;
  };

  config.home.packages = [ libnotify ];

  services.dunst = {
    enable = true;
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper";
    settings = {
      global = {
        font = "DejaVu Sans";
        markup = true;
        format = "<b>%s</b>\\n%b%p";
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
      play_sound = {
        summary = "*";
        script = "${homeDir}/${soundpath}/boop.sh";
      };
    };
  };
}
