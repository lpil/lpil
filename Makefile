export PATH := ./node_modules/.bin:$(PATH)

help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


test: ## Run the test watcher
	pulp --watch test
.PHONY: test


test-once: ## Run the tests
	pulp test
.PHONY: test-once


install: ## Install deps
	yarn install
	bower install
.PHONY: install


repl: ## Run the REPL
	pulp repl
.PHONY: repl


run: ## Run the project
	pulp run
.PHONY: run
