# NixOS Configs

These are my Nix and Home Manager configs. They include dotfiles and things like
that that have been ported over. This allows for incredibly portable and
automated builds of the exact same system configuration and RICE across all
systems.

## Usage

This is not going to be all that useful for anyone other than me since you don't
have the machines configured in this repo.

0. After running `nixos-generate`, edit `configuration.nix` to enable flakes:

```
  nix.settings = {
    experimental-features = [ "flakes" "nix-command" ];
  };

```

1. Follow the manual up to where you `nixos-install`
2. Use `nix-enter` to chroot into your newly installed system.
3. `su <user>` to login as your user if you want to.
4. `git clone` this repo somewhere.
5. `cd` into the `nixos` directory, and `nixos-rebuild boot --flake .#<config id>`
6. Reboot.

## How it works

There is one way of providing robust configs for multiple systems, and that is
by writing nixpkgs-style modules with options. However that is a bit overkill
for a few reasons:

1. Most systems use the same options for the same module. For example I am
   not going to install vim with different configs on two systems.
2. If there is really a need to alter anything, I can do so with a flag. The
   number of alternatives used by me is not that big, so usually its just one or
   two flags.

Thus the config structure I have chosen is just to write a whole bunch of
different functions that will then be inserted into each machine's `import`
block. To disable a certain "module", just comment it out. Each function should
be responsible for its own functioning (haha), such as pulling in dependencies,
etc. This means all you need to enable some service, software, etc. is to just
import it.

Some disadvantages:

1. It's not that flexible since you are assuming all machines are going to use
   the same file. Any changes have to be provided through flags.
2. Sometimes even that gets messy, and you need to split configs up, or just not
   have a global config for all machines. For example grub BIOS and EFI configs
   are harder to share globally.
