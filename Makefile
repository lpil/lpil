help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

clean: ## Remove compiled output
	rm -fr dist

start: ## Start dev servers
	./node_modules/.bin/parcel serve src/index.html &
	./node_modules/.bin/parcel watch src/dev-server.js --target node &
	./node_modules/.bin/nodemon dist/dev-server.js

build: dist/server/server.js dist/site/index.html ## Compile all

dist/server/server.js:
	./node_modules/.bin/parcel build src/server.js --target node --out-dir dist/server

dist/site/index.html:
	./node_modules/.bin/parcel build src/index.html --out-dir dist/site
