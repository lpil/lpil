{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "67.207.67.2"
      "67.207.67.3"
    ];
    defaultGateway = "165.22.112.1";
    defaultGateway6 = "";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce true;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="165.22.117.176"; prefixLength=20; }
          { address="10.16.0.5"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="fe80::58fa:5eff:fe05:8c4d"; prefixLength=64; }
        ];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="5a:fa:5e:05:8c:4d", NAME="eth0"
  '';
}
