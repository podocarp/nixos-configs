{ pkgs, config, homeDir, ... }:

let
  # This should match the script
  soundpath = ".config/dunst";
in
{
  # Sound is taken from KDE repo.
  home.file."${soundpath}/boop.ogg".source = builtins.fetchurl {
    url = "https://invent.kde.org/plasma/oxygen-sounds/-/raw/master/sounds/Oxygen-Sys-App-Message.ogg";
    sha256 = "1kr06mzn59c0w4g52pkwxgbhds912a08fwsmg0fsmf0c66bmx8w2";
  };
  home.file."${soundpath}/boop.sh" = {
    text = "${pkgs.pulseaudio}/bin/paplay ${homeDir}/${soundpath}/boop.ogg";
    executable = true;
  };

  home.packages = [ pkgs.libnotify ];

  services.dunst = {
    enable = true;
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper";
    settings = {
      global = {
        font = "DejaVu Sans";
        markup = true;
        format = "<u>%a</u>\\n<b>%s</b>\\n%b%p";
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
        browser = "${pkgs.chromium}/bin/chromium";
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
      change_volume = {
        appname = "changevolume";
        history_ignore = true;
      };
    };
  };
}
