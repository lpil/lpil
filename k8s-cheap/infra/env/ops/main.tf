#
# A cluster for developing and testing changes to the cluster itself
# before they are applied to the production cluster.
#

terraform {
  backend "remote" {
    organization = "lpil"

    workspaces {
      name = "learning-k8s-cheap--infra--ops"
    }
  }
}

provider "google-beta" {
  project = "gcp-simple-app-dev"
  region  = "europe-west1"
  zone    = "europe-west1-c"
  version = "~> 2.5"
}

provider "google" {
  project = "gcp-simple-app-dev"
  region  = "europe-west1"
  zone    = "europe-west1-c"
  version = "~> 2.5"
}

provider "template" {
  version = "~> 2.1"
}

module "infra" {
  source = "../../modules/infra"
  region = "europe-west1"
  env    = "ops"
}
