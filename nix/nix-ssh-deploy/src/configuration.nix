{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect
    
  ];

  boot.cleanTmpDir = true;
  networking.hostName = "purple";
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC7N0vfAlxZLwJHVD5Qp0navxgfhyT/BnhX2CLLS9ukjIEnUXwnKwrKKjLpaZidGVuOezUnM+8kNPTXtl71Ki6VoXvtizf8ow8Wg4GH7VgdZnnjseK8ODLRF8HLH3JrsMceqjkDXmAVXLn38u7/5fdIobpvtQ9bxQdt760bvnQxU+syle1VWdQTKRXjqjydtsziYSylWt50TAyDxGUfwtyEyf+aWtPZERTN5r9TzuXnY/se0YM1x3CWepJb7uKsL2iX3rQ8KnUGvc3FsGK0Be9rgLeE+J3dAPCbdeSyNzNYVWwyUvmc0mCT2RLMmeMhIPjbQaba8OkANHNW1cN2RWs99q7V3RukdyAn5eO6j0CVANmcRoT68IBNH9z/tQIpngIcvQMeLot571C/QSmb5WLtNiXj2upuuiSGFsr+dRjBUn+WGGiqpRRMB1vDun1973cNqGwtG5ZAJBQENaf3o+JQCs1VgcH6+Ia1SF0osHJaZCgDS9cvH6+x7Jlu2dzGoQnpTnvhrnZunPVIreh8Tvk45IdjC9teioZVb96/IM/dKPZjZMpV8oQjybB7nrYUM+mxIr0oNhYShQLwMPflHPWgjCvLTIM4gJGdJZ5ggaAUYilQKcapnSZM+iKtmiIqrKsbQeh9c+uedJnOwnBPbGedaMgGuOME7LGMs8eWgYo6jw== louis@lpil.uk"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQClQlourZpevGFtUoufHbnNIX/9kn2z/2mUrAgLHj26+51+8tYaHXmumO7M0CESe68ATYfi97whtwcn5PcKQQNthjnSy6cZGyGdKfOZ0aFf4ealnduJiKhBNJPTbp1fzPJ5wENzjBqwT92hLzTkz5sNZLCK3Wg9ecD58G4GYXWjCZRZuG+B+Vnl2wzTEFJiBnvpfN2eM8PJo8qF0j1vmy/6I0fAXql8OTAQe5YBQBJvizBvb546AeJW+Qq6CvNfOs8ISPqfd3F8vt+gAkHLwLRe2kQGJ370PEvWFGlDxuwCzGEc/Nv+oTSoAS3iz1ICX7DAgF/lDHGXLgkzOI4hEXatZuKU3Y+Ii1+ffD+KyNeCTzPfE8pT2LXg0mjBKlM/X4UUBMzPc/0O2OxicwSvVLZLWB3ZACvAF/YTgAzuzpRo4rfMZNADPI9TVMRHyrrnAx5cl3vS3mHJWPqI/jBQ3sPTEaZCBsNzbASSeMKiFMRZEK33OIS49BZJ6YRw9kxz9zz+/9DWNer59jkgMSp1bZCMMLC4npeKgzwWfWRsNBEs1DLSp4/eNdKQLmk8ecDff7hFL2odC0dBBYk14QmK4mpjGNHQxQEiu82a4gtWFFFqoUraJxUsOtOVUwhku7uP0Eb3kHSAHs+2IDmuvQx40fChC6AgZQcJv0wAVioKcf5L8w== louis@lpil.uk"
  ];
}
