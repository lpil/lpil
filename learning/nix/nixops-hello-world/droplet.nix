let
  webServer = { config, pkgs, ... }: {
    deployment.targetEnv = "digitalOcean";
    deployment.digitalOcean.enableIpv6 = true;
    deployment.digitalOcean.region = "lon1";
    deployment.digitalOcean.size = "s-1vcpu-1gb";

    networking.firewall.allowedTCPPorts = [ 80 ];

    # ssh
    services.openssh.enable = true;

    # Traefik reverse proxy
    services.traefik = {
      enable = true;

      # traefik.toml config
      configOptions = {
        defaultEntryPoints = ["http"];
        entryPoints.http.address = ":80";

        # Enable /ping endpoint
        ping.entryPoint = "http";
      };
    };
  };
in
{
  network.description = "Test digital ocean network";

  resources.sshKeyPairs.ssh-key = {};

  web1 = webServer;
}
