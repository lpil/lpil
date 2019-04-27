locals {
  container_port = "80"
  vps_port = "80"
}

# The template with which VPS instances are created to serve the application.
#
resource "google_compute_instance_template" "app" {
  machine_type = "f1-micro"
  project      = "${var.project}"

  metadata = {
    startup-script = "${data.template_file.startup_script.rendered}"
  }

  tags = ["allow-ssh", "app"]

  labels = {
    env = "${var.env}"
  }

  network_interface {
    network = "default"
  }

  disk {
    source_image = "cos-cloud/cos-73-lts"
    auto_delete  = true
    boot         = true
  }

  service_account {
    scopes = []
  }

  lifecycle {
    create_before_destroy = true
  }
}

data "template_file" "startup_script" {
  template = "${file("${path.module}/startup_script.template.sh")}"

  vars {
    DOCKER_TAG = "tutum/hello-world"
    DOCKER_PORT = "${local.container_port}"
    VPS_PORT = "${local.vps_port}"
  }
}

# A group manager that creates a group of application VPS instances from the
# template.
#
resource "google_compute_region_instance_group_manager" "app" {
  provider           = "google-beta"
  name               = "app-instance-group-manager"
  base_instance_name = "app"
  region             = "${var.region}"
  target_size        = "${var.replicas}"
  target_pools       = ["${google_compute_target_pool.app.self_link}"]

  update_policy {
    type                  = "PROACTIVE"
    minimal_action        = "REPLACE"
    max_surge_fixed       = 3
    max_unavailable_fixed = 0
    min_ready_sec         = 10
  }

  named_port {
    name = "http"
    port = "${local.vps_port}"
  }

  # auto_healing_policies {
  #   health_check      = "${google_compute_health_check.autohealing.self_link}"
  #   initial_delay_sec = 120
  # }

  version {
    name              = "app"
    instance_template = "${google_compute_instance_template.app.self_link}"
  }
}

## A health check which which the group manager checks if an instance is up
##
#resource "google_compute_health_check" "autohealing" {
#  name                = "autohealing-health-check"
#  check_interval_sec  = 5
#  timeout_sec         = 5
#  healthy_threshold   = 2
#  unhealthy_threshold = 10                         # 50 seconds

#  http_health_check {
#    request_path = "/healthz"
#    port         = "8080"
#  }
#}

# Specified which instances the load balancer should send traffic to
#
resource "google_compute_target_pool" "app" {
  name             = "app-target-pool"
  session_affinity = "NONE"

  # health_checks = [
  #   "${google_compute_http_health_check.default.name}",
  # ]
}

# The load balancer that sends traffic to our app instances
#
resource "google_compute_forwarding_rule" "app" {
  name                  = "app"
  target                = "${google_compute_target_pool.app.self_link}"
  # load_balancing_scheme = "EXTERNAL"
  port_range            = "${local.vps_port}"
}

# Allow traffic to the app instances so that the  load balancer can use them
#
resource "google_compute_firewall" "app" {
  name    = "app-lb"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["${local.vps_port}"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["app"]
}
