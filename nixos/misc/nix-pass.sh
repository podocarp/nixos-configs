#! /bin/sh

f=$(mktemp)
trap "rm $f" EXIT
su -c "pass show $1" pengu > $f
nix-instantiate --eval --expr "builtins.readFile $f"
