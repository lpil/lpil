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
  resources = concat(
    [
      for web in digitalocean_droplet.web :
      web.urn
    ],
    [
      digitalocean_loadbalancer.public.urn
    ]
  )
}

resource "digitalocean_loadbalancer" "public" {
  name        = "loadbalancer"
  region      = "lon1"

  forwarding_rule {
    entry_port     = 80
    entry_protocol = "http"

    target_port     = 80
    target_protocol = "http"
  }

  healthcheck {
    port                   = 80
    protocol               = "http"
    path                   = "/"
    check_interval_seconds = "5"
  }
}

resource "digitalocean_droplet" "web" {
  count = 2

  image      = "coreos-stable"
  name       = "web-${count.index}"
  region     = "lon1"
  size       = "s-1vcpu-1gb"
  tags       = []
  ssh_keys   = ["4b:6f:c5:0a:41:3d:36:81:92:8e:84:52:61:44:d6:a8"]
  depends_on = [digitalocean_loadbalancer.public]

  lifecycle {
    create_before_destroy = true
  }

  user_data = <<EOF
#cloud-config

coreos:
  units:
    - name: "web.service"
      command: "start"
      content: |
        [Unit]
        Description=web
        After=docker.service
        Requires=docker.service

        [Service]
        TimeoutStartSec=0
        ExecStartPre=/usr/bin/docker pull hashicorp/http-echo
        ExecStart=/usr/bin/docker run -p 80:80 --name web hashicorp/http-echo -text="yellow ${count.index}" -listen=:80

        [Install]
        WantedBy=multi-user.target
EOF

  # Wait for web service to come online
  provisioner "local-exec" {
    command = "./check_health.sh ${self.ipv4_address}"
  }

  # Register service with LB
  provisioner "local-exec" {
    command = "doctl compute load-balancer add-droplets ${digitalocean_loadbalancer.public.id} --droplet-ids ${self.id}"
  }

  # Wait a little to ensure LB health check is passing
  provisioner "local-exec" {
    command = "sleep 30"
  }
}
