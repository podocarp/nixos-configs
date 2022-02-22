# NixOS Configs

These are my Nix and Home Manager configs. They include dotfiles and things like
that that have been ported over. This allows for incredibly portable and
automated builds of the exact same system configuration and RICE across all
systems.
Also there are some services that I run on my server.
Explanations for the server side configs can be found here: http://jiaxiaodong.com/blog/computing/server/nix/sum/

## Usage

First install as per the manual to get a bare system.
Boot into your new system.
Clone this repo.
Symlink the entire `nixos` folder in this repo to `/etc/nixos`.
Edit `configuration.nix` to source the right files.
An example is provided.

Add the appropriate channels that you need.
Note that the following would require you to move to `nixos-unstable` as well.
```
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --add https://github.com/Mic92/sops-nix/archive/master.tar.gz sops-nix
nix-channel --update
```

Then just `nixos-rebuild switch`.
