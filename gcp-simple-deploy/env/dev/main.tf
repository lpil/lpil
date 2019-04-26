terraform {
  backend "remote" {
    organization = "lpil"

    workspaces {
      name = "gcp-simple-deploy-dev"
    }
  }
}

# Need to use beta for compute_region_instance_group_manager.update_policy
provider "google-beta" {
  project = "gcp-simple-app-dev"
  region  = "europe-west1"
  zone    = "europe-west1-c"
  version = "~> 2.5"
}

module "the-app" {
  source   = "../../modules/app"
  env      = "dev"
  project  = "gcp-simple-app-dev"
  region   = "europe-west1"
  replicas = "2"
}
