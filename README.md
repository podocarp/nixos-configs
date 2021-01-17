# NixOS Configs

![Screenshot](./screenshots/ss1.png)

These are my Nix and Home Manager configs. They include dotfiles and things like
that that have been ported over.

## Usage

Things in the folder `nixpkgs` are supposed to go into `~/.config/nixpkgs`. The
file `configuration.nix` goes to `/etc/nixos/configuration.nix`. Afterwards run
`nixos-rebuild switch` and `home-manager switch`.
