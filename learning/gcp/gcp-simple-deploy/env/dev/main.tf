terraform {
  backend "remote" {
    organization = "lpil"

    workspaces {
      name = "gcp-simple-deploy-dev"
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

module "the-app" {
  source   = "../../modules/app"
  env      = "dev"
  project  = "gcp-simple-app-dev"
  region   = "europe-west1"
  replicas = "2"
}
