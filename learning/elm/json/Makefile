BIN=./node_modules/.bin

ELM_TEST=$(BIN)/elm-test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

test: ## Run the frontend tests
	$(ELM_TEST)

.PHONY: \
	test \
	help
