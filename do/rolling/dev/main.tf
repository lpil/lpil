terraform {
  backend "remote" {
    organization = "lpil"

    workspaces {
      name = "learning-do-rolling-dev"
    }
  }
}

provider "digitalocean" {}

 resource "digitalocean_project" "project" {
  name        = "learning-do-rolling-dev"
  description = "A test project!"
  purpose     = "web-app"
  environment = "development"
  resources   = [
    for web in digitalocean_droplet.web:
    web.urn
  ]
}

resource "digitalocean_droplet" "web" {
  count = 2

  image    = "coreos-stable"
  name     = "web-${count.index}"
  region   = "lon1"
  size     = "s-1vcpu-1gb"
  tags     = []
  ssh_keys = ["4b:6f:c5:0a:41:3d:36:81:92:8e:84:52:61:44:d6:a8"]

  lifecycle {
    create_before_destroy = true
  }

  user_data = <<EOF
#cloud-config

coreos:
  units:
    - name: "docker-redis.service"
      command: "start"
      content: |
        [Unit]
        Description=web
        After=docker.service
        Requires=docker.service

        [Service]
        TimeoutStartSec=0
        ExecStartPre=-/usr/bin/docker kill web
        ExecStartPre=-/usr/bin/docker rm web
        ExecStartPre=/usr/bin/docker pull hashicorp/http-echo
        ExecStart=/usr/bin/docker run -p 80:80 --name web hashicorp/http-echo -text=yellow -listen=:80

        [Install]
        WantedBy=multi-user.target
EOF

  #   provisioner "local-exec" {
  #     command = "./check_health.sh ${self.ipv4_address}"
  #   }
}

# resource "digitalocean_loadbalancer" "public" {
#   name        = "loadbalancer-1"
#   region      = "lon1"
#   droplet_tag = "zero-downtime"

#   forwarding_rule {
#     entry_port     = 80
#     entry_protocol = "http"

#     target_port     = 80
#     target_protocol = "http"
#   }

#   healthcheck {
#     port                   = 80
#     protocol               = "http"
#     path                   = "/"
#     check_interval_seconds = "5"
#   }
# }

# output "lb_ip" {
#   value = "${digitalocean_loadbalancer.public.ip}"
# }
