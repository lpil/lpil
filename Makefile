serve: ## Run the development server
	bundle exec jekyll server --watch --safe

build: clean ## Compile the site
	JEKYLL_ENV=production bundle exec jekyll build

debug: clean ## Run the development server in verbose mode
	JEKYLL_ENV=production bundle exec jekyll build --verbos

clean: ## Delete compiled output
	rm -rf _site

help: ## Prints help for targets with comments
	@echo lpil.uk - The site of Louis Pilfold
	@echo
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


.PHONY: \
	build \
	clean \
	debug \
	help  \
	serve
