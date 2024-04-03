# https://cloud.google.com/deployment-manager/docs/quickstart

DEPLOYMENT_NAME=goggles-deployment

deployment-create: ## Create the deployment in GCP Deployment Manager
	gcloud deployment-manager deployments create $(DEPLOYMENT_NAME) --config infra/deployment.yml
.PHONY: deployment-create


deployment-update: ## Update a deployment with the current yml
	gcloud deployment-manager deployments update $(DEPLOYMENT_NAME) --config infra/deployment.yml
.PHONY: deployment-update


deployment-describe: ## View details of deployment
	gcloud deployment-manager deployments describe $(DEPLOYMENT_NAME)
.PHONY: deployment-describe


deployment-delete: ## Delete deployment
	gcloud deployment-manager deployments delete $(DEPLOYMENT_NAME)
.PHONY: deployment-delete
