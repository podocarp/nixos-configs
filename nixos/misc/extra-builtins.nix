{ exec, ... }:
{
  getSecret = name: exec [ ./nix-pass.sh name ];
}
