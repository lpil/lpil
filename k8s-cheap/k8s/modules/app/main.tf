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
      }
    }

    template {
      metadata {
        labels {
          workload = "app"
        }
      }

      spec {
        container {
          image = "hashicorp/http-echo"
          name  = "app"
          args  = ["-text=blue", "-listen=:80"]
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
    }

    port {
      port        = 8080
      target_port = 80
    }

    type = "NodePort"
  }
}
