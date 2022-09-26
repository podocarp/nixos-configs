{ pkgs, ... }:
{
  programs.ranger = {
    enable = true;
    loadDefaultRc = false;
    config = ./rc.conf;
    rifle = ./rifle.conf;
    scope = ./scope.sh;
    commands = ./commands.py;
    extraPackages = [ pkgs.ueberzug ];
    plugins = [
      {
        name = "ranger_devicons";
        path = builtins.fetchGit "https://github.com/alexanderjeurissen/ranger_devicons";
      }
    ];
  };

  programs.bash.initExtra = ''
    if [ -n "$RANGER_LEVEL" ]; then export PS1="[ranger]$PS1"; fi

    __ranger() {
        if [ -z "$RANGER_LEVEL" ]; then
            /usr/bin/env ranger "$@"
        else
            exit
        fi
    }

    ranger() {
        temp_file="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
        __ranger --choosedir="$temp_file" -- "''${@:-$PWD}"
        if chosen_dir="$(cat -- "$temp_file")" && [ -n "$chosen_dir" ] && [ "$chosen_dir" != "$PWD" ]; then
            cd -- "$chosen_dir"
        fi
        rm -f -- "$temp_file"
    }
  '';
}
