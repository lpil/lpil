# TODO: forwarding rule, url map, HTTP proxy

# https://www.terraform.io/docs/providers/google/r/compute_managed_ssl_certificate.html

locals {
  container_port = "80"
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
    DOCKER_TAG  = "tutum/hello-world"
    DOCKER_PORT = "${local.container_port}"
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

  update_policy {
    type                  = "PROACTIVE"
    minimal_action        = "REPLACE"
    max_surge_fixed       = 3
    max_unavailable_fixed = 0
    min_ready_sec         = 10
  }

  named_port {
    name = "http"
    port = "80"
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

# A health check for application instances
#
resource "google_compute_health_check" "app" {
  name                = "app"
  check_interval_sec  = 5
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 2

  http_health_check {
    request_path = "/"
    port         = "80"
  }
}

# A backend service for the load balancer to sent traffic to.
# Made up of the application instances.
#
resource "google_compute_backend_service" "app" {
  name          = "backend-service"
  health_checks = ["${google_compute_health_check.app.self_link}"]
}

# The load balancer that sends traffic to our app instances
#
resource "google_compute_forwarding_rule" "app" {
  name            = "app"
  ports           = ["80"]
  backend_service = "${google_compute_backend_service.app.self_link}"

  # load_balancing_scheme = "EXTERNAL"
}
