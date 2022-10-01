{ pkgs, ... }:
{
  programs.ranger = {
    enable = true;
    loadDefaultRc = false;
    config = ./rc.conf;
    rifle = ./rifle.conf;
    scope = ./scope.sh;
    commands = ./commands.py;
    package = pkgs.ranger.overrideAttrs (super: {
      src = pkgs.fetchFromGitHub {
        owner = "ranger";
        repo = "ranger";
        rev = "master";
        sha256= "1rygfryczanvqxn43lmlkgs04sbqznbvbb9hlbm3h5qgdcl0xlw8";
      };
    });
    extraPackages = with pkgs; [
      ueberzug
      ffmpegthumbnailer
      trash-cli
    ];
    plugins = [
      {
        name = "ranger_devicons";
        path = pkgs.fetchFromGitHub {
          owner = "alexanderjeurissen";
          repo = "ranger_devicons";
          rev = "master";
          sha256= "1rygfryczanvqxn43lmlkgs04sbqznbvbb9hlbm3h5qgdcl0xlw8";
        };
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
