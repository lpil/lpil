export PATH := ./node_modules/.bin:$(PATH)


help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help


build: ## Compile project
	bsb -make-world
.PHONY: build


build-watch: ## Compile project when files change
	bsb -make-world -w
.PHONY: build-watch


# TODO: Reloading with nodemon
start: ## Start a dev server
	node lib/js/src/devServer.js
.PHONY: start


clean: ## Remove compiled artifacts
	bsb -clean-world
	rm -fr dist
.PHONY: clean


deploy: clean build ## Deploy to GCP Cloud Functions
	mkdir dist
	cp package.json dist/
	cp -r ./node_modules/bs-platform/lib/js dist/bs-platform
	cp lib/js/src/* dist/
	find dist -type f -exec sed -i 's|bs-platform/lib/js|./bs-platform|g' {} +
	./bin/deploy.sh
.PHONY: deploy
