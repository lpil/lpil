# The template with which VPS instances are created to serve the application.
#
resource "google_compute_instance_template" "app" {
  machine_type = "f1-micro"
  project      = "${var.project}"

  metadata = {
    startup-script = "${data.template_file.startup_script.rendered}"
  }

  labels = {
    env = "${var.env}"
  }

  network_interface {
    network = "default"
  }

  disk {
    source_image = "cos-cloud/cos-73-lts"
    auto_delete = true
    boot        = true
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
    # COLOR = "blue"
    # COLOR = "green"
    DOCKER_TAG = "tutum/hello-world"
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

