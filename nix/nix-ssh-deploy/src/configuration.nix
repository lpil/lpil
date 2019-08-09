{ pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix
    ./ssh.nix
  ];

  environment.systemPackages = [
    pkgs.htop
  ];

  boot.cleanTmpDir = true;
  networking.hostName = "purple";
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 80 8080 443 ];

  # Traefik reverse proxy
  services.traefik.enable = true;
  services.traefik.configFile = "/etc/traefik/traefik.toml";
  environment.etc."traefik/traefik.toml".text = ''
[entryPoints]
  [entryPoints.http]
  address = ":80"
  [entryPoints.https]
  address = ":443"

[file]
watch = true
filename = "/etc/traefik/watched_config.toml"
'';

  environment.etc."traefik/watched_config.toml".text = ''
[backends]
  [backends.happylabs-api]
    [backends.happylabs-api.servers]
      [backends.happylabs-api.servers.server0]
        url = "http://localhost:3000"

[frontends]
  [frontends.happylabs-api]
    entryPoints = ["http", "https"]
    backend = "happylabs-api"

    [frontends.happylabs-api.routes]
      [frontends.happylabs-api.routes.route0]
        rule = "Host:happylabs-api-purple.lpil.uk"
  '';

  virtualisation.docker.enable = true;

  docker-containers.happylabs-api = {
    image = "lpil/happylabs-api-rust:11";
    ports = ["3000:3000"];
    cmd = ["-text='Happylabs'"];
  };
}
