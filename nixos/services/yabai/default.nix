{ pkgs, ... }:
{
  services.yabai = {
    enable = true;
    enableScriptingAddition = true;
    package = pkgs.yabai.overrideAttrs rec {
      version = "7.0.3";
      src = pkgs.fetchzip {
        url = "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
        sha256 = "sha256-EvtKYYjEmLkJTnc9q6f37hMD1T3DBO+I1LfBvPjCgfc=";
      };
    };
    config = {
      focus_follows_mouse = "autofocus";
      active_window_border_color = "0xffff0000";
      normal_window_border_color = "0xff555555";
      window_border_width = "3";
      window_border = "on";
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      mouse_drop_action = "swap";
      layout = "bsp";
    };
    extraConfig = ''
      yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
      sudo yabai --load-sa
    '';
  };
}
