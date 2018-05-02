terraform {
  backend "s3" {
    # TODO: Configurable
    bucket = "lpil-blondie-terraform-state"
    key    = "blondie"
    region = "eu-west-1"
  }
}

provider "aws" {
  # access_key = "${var.aws_access_key}"
  # secret_key = "${var.aws_secret_key}"
  region = "eu-west-1"
}

module "blondie_dev" {
  source = "./terraform"
  env    = "dev"
}

module "blondie_staging" {
  source = "./terraform"
  env    = "staging"
}

module "blondie_prod" {
  source = "./terraform"
  env    = "prod"
}
