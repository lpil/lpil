NBIN=./node_modules/.bin

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


start: node_modules elm-stuff ## Start the dev server
	@NODE_ENV=development $(NBIN)/webpack-dev-server \
		--hot --inline --content-base src/ --no-info --colors

build: dist ## Compile the project

clean: ## Remove compiled files
	rm -rf dist


#
# Files
#

node_modules:
	yarn install

elm-stuff: node_modules
	$(NBIN)/elm-package install --yes

dist dist/index.html: node_modules elm-stuff
	NODE_ENV=production $(NBIN)/webpack -p


.PHONY: \
	start \
	clean \
	build \
	help
