{ config, port, ... }:
{
  containers.gollum = {
    autoStart = true;
    ephemeral = true;
  };
}
