help: ## Prints help for targets with comments
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

start: ## Run the development server
	bundle exec jekyll server --watch --safe --port 3000 --host 0.0.0.0

build: clean ## Compile the site
	JEKYLL_ENV=production bundle exec jekyll build

debug: clean ## Run the development server in verbose mode
	JEKYLL_ENV=production bundle exec jekyll build --verbos

clean: ## Delete compiled output
	rm -rf _site

quack:
	@echo "Quack quack!"

.PHONY: \
	build \
	clean \
	debug \
	help  \
	start
