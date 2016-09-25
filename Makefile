BIN=./node_modules/.bin

WEBPACK=$(BIN)/webpack
WEBPACK_SERVER=$(BIN)/webpack-dev-server
ELM_PACKAGE= $(BIN)/elm-package
ELM_TEST=$(BIN)/elm-test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install: ## Install deps
	npm install
	$(ELM_PACKAGE) install --yes
	cd tests && ../$(ELM_PACKAGE) install --yes
	@echo 'Done! <3 <3 <3'

dev: ## Start the frontend dev server
	NODE_ENV=development $(WEBPACK_SERVER) --hot --inline --content-base src/, --no-info --colors

test: ## Run the frontend tests
	$(ELM_TEST)

build: ## Compile the app
	rm -rf dist
	NODE_ENV=production $(WEBPACK) -p

.PHONY: \
	install \
	build \
	test \
	help \
	dev
