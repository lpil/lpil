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

module "dev_blondie" {
  source = "./terraform"
  env    = "dev"
}

module "staging_blondie" {
  source = "./terraform"
  env    = "staging"
}

module "prod_blondie" {
  source = "./terraform"
  env    = "prod"
}
