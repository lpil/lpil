locals {
  namespace = "app-${var.env}"
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
    replicas = 3

    selector {
      match_labels {
        workload = "app"
        role     = "web_server"
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
          args  = ["-text=purple", "-listen=:80"]
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
      target_port = 80
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
