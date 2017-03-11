NBIN=$(shell pwd)/node_modules/.bin
ELM_TEST=$(NBIN)/elm-test --compiler $(NBIN)/elm-make


help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help


install: ## Install deps
	yarn install
	$(NBIN)/elm-package install --yes
.PHONY: install


start: ## Start the dev server
	@NODE_ENV=development $(NBIN)/webpack-dev-server \
					 --hot \
					 --inline \
					 --content-base src/, \
					 --no-info \
					 --colors \
					 --config ./webpack.dev.config.js
.PHONY: start


build: ## Compile app
	@NODE_ENV=production $(NBIN)/webpack \
					 --optimize-minimize \
					 --define \
					 --progress \
					 --bail \
					 --config ./webpack.prod.config.js
.PHONY: build


clean: ## Remove compiled artifacts
	rm -rf dist
.PHONY: clean


test: ## Run the front end tests
	$(ELM_TEST)
.PHONY: elm-test


test-watch: ## Run the front end test watcher
	$(ELM_TEST) --watch
.PHONY: elm-test-watch
