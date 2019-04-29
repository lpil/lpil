terraform {
  backend "remote" {
    organization = "lpil"

    workspaces {
      name = "learning-k8s-cheap--k8s--dev"
    }
  }
}

provider "kubernetes" {
  version = "~> 1.6"
}

module "infra" {
  source = "../../modules/app"
  env    = "dev"
}
