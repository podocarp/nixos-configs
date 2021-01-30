# NixOS Configs

![Screenshot](./screenshots/ss1.png)

These are my Nix and Home Manager configs. They include dotfiles and things like
that that have been ported over. This allows for incredibly portable and
automated builds of the exact same system configuration and RICE across all my
systems.

## Usage

First install as per the manual. However, before the final build step, run
```
nixos-prefetch-url --unpack https://github.com/podocarp/nixos-configs
```
Then visit the path it was extracted to, and copy the desired `.nix` from the
`nixos/` folder. This simplifies things and prevents you from making mistakes
like forgetting to enable wireless support and having to reboot. This also adds
an user you can log into. HOWEVER please read the configs before adding. Certain
configs contain settings such as undervolting and allowing non-free software you
might not want.

After rebooting into your successful install, log into root and `passwd` the
non-root user. Then log into that user.

Install home-manager:
```
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update
nix-env '<home-manager>' -A install
```

Next, you want to get this repo to a proper place. The original `prefetch`
will get GC'd after some time.
```
cd ~/Documents
git clone http://github.com/podocarp/nixos-configs
```

Finally, you will `ln -s` the files to where they are required. (NOTE: if
`/home` and `/` are on different partitions the symbolic link cannot be created,
and you will have to find another way). Suppose you want config `x.nix`, then
you will do something like
```
ln -s ~/Documents/nixos-configs/nixos/x.nix /etc/nixos/x.nix
ln -s ~/Documents/nixos-configs/nixos/common.nix /etc/nixos/common.nix
cat << EOF > /etc/nixos/configuration.nix
{
    imports = [
      ./x.nix
      ./hardware-configuration.nix # This should have been created by the installer
    ];
}
EOF
```

## Configured things

Check within `nixpkgs/programs` and `nixpkgs/services`. I suggest you use this as 
guide instead of as a drop-in replacement. Some things I recommend changing:

- nvim configurations
- Firefox's hardened user.js
- vifm sixel preview uses XTerm
