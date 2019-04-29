locals {
  namespace      = "app-${var.env}"
  container_port = 80
}

resource "kubernetes_namespace" "app" {
  metadata {
    name = "${local.namespace}"
  }
}

resource "kubernetes_deployment" "app" {
  metadata {
    name      = "app"
    namespace = "${local.namespace}"

    labels {
      workload = "app"
    }
  }

  spec {
    replicas = 2

    selector {
      match_labels {
        workload = "app"
        role     = "web_server"
      }
    }

    strategy {
      type = "RollingUpdate"

      rolling_update = {
        max_unavailable = "1%"
        max_surge       = 2
      }
    }

    template {
      metadata {
        labels {
          workload = "app"
          role     = "web_server"
        }
      }

      spec {
        container {
          image = "hashicorp/http-echo"
          name  = "app"
          args  = ["-text=yellow", "-listen=:80"]

          readiness_probe {
            initial_delay_seconds = 10
            period_seconds        = 5

            http_get {
              path = "/ready"
              port = "${local.container_port}"
            }
          }

          liveness_probe {
            initial_delay_seconds = 10
            period_seconds        = 5

            http_get {
              path = "/alive"
              port = "${local.container_port}"
            }
          }
        }
      }
    }
  }
}

resource "kubernetes_service" "app" {
  metadata {
    name      = "app"
    namespace = "${local.namespace}"
  }

  spec {
    selector {
      workload = "app"
      role     = "web_server"
    }

    port {
      port        = 8080
      target_port = "${local.container_port}"
    }

    type = "NodePort"
  }
}

# Amazingly this terraform provider does not support ingress resources
# https://github.com/terraform-providers/terraform-provider-kubernetes/issues/14
resource "null_resource" "app_ingress" {
  triggers = {
    manifest_sha1 = "${sha1("${data.template_file.app_ingress.rendered}")}"
  }

  provisioner "local-exec" {
    command = "kubectl apply -f -<<EOF\n${data.template_file.app_ingress.rendered}\nEOF"
  }
}

data "template_file" "app_ingress" {
  template = "${file("${path.module}/app_ingress.template.yml")}"

  vars {
    NAMESPACE    = "${local.namespace}"
    SERVICE_NAME = "${kubernetes_service.app.metadata.0.name}"
    SERVICE_PORT = "8080"
  }
}
