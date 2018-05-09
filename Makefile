help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help

clean: ## Remove compiled output
	rm -fr dist
.PHONY: clean

start: ## Start dev servers
	./node_modules/.bin/parcel serve src/index.html &
	./node_modules/.bin/parcel watch src/dev-server.js --target node &
	./node_modules/.bin/nodemon dist/dev-server.js
.PHONY: start

build: dist/server/index.js dist/site/index.html ## Compile all
.PHONY: build

deploy: clean dist/server/index.js dist/site/index.html ## Deploy functions
	cp -r package.json dist/server/
	bin/deploy.sh
.PHONY: deploy

dist/server/index.js:
	./node_modules/.bin/parcel build src/server.js \
		--target node \
		--out-dir dist/server \
		--out-file index.js

dist/site/index.html:
	./node_modules/.bin/parcel build src/index.html \
		--public-url '.' \
		--out-dir dist/site
