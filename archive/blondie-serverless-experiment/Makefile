# We default to the dev env
ENV ?= dev

help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
	@echo
	@echo 'Use `make command ENV=prod` to target non-dev env'

server: ## Start dev server
	./node_modules/.bin/webpack-dev-server --quiet &
	./node_modules/.bin/bsb -make-world -w
.PHONY: start

start: ## Start dev compilers
	./node_modules/.bin/webpack --watch --display minimal &
	./node_modules/.bin/bsb -make-world -w
.PHONY: start

test: ## Run test watcher
	./node_modules/.bin/jest --watch
.PHONY: test

test-once: ## Run tests once
	./node_modules/.bin/jest
.PHONY: test-once

test-ci:
	./node_modules/.bin/jest --ci
.PHONY: test-ci

build: ## Compile app code
	./node_modules/.bin/bsb -make-world
	NODE_ENV=production ./node_modules/.bin/webpack
.PHONY: build

clean: ## Wipe compiled assets
	./node_modules/.bin/bsb -clean-world
	rm -fr dist
.PHONY: clean

clean-all: clean ## Wipe all local files
	rm -fr .terraform node_modules
.PHONY: clean-all

tf-plan: .terraform dist/tick_function.zip dist/api_function.zip ## Plan terraform update
	terraform plan -target=module.$(ENV)_blondie
.PHONY: tf-plan

tf-apply: .terraform dist/tick_function.zip dist/api_function.zip ## Apply terraform update
	terraform apply -target=module.$(ENV)_blondie
.PHONY: tf-apply

tf-destroy: .terraform dist/tick_function.zip dist/api_function.zip ## Teardown all infra (!!!)
	terraform destroy -target=module.$(ENV)_blondie
.PHONY: tf-destroy

tf-output: .terraform ## View terraform output values
	terraform output -module=$(ENV)_blondie
.PHONY: tf-output

logs-tick: ## Tail logs of the tick function
	./node_modules/.bin/cwtail --follow /aws/lambda/$(ENV)_blondie_tick
.PHONY: logs-tick

logs-api: ## Tail logs of the api function
	./node_modules/.bin/cwtail --follow /aws/lambda/$(ENV)_blondie_api
.PHONY: logs-api

dist/api_function.zip: build
	cd dist && zip -r api_function.zip api_function.js

dist/tick_function.zip: build
	cd dist && zip -r tick_function.zip tick_function.js

.terraform:
	terraform init
	terraform get
