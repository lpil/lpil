repl: ## Start REPL
	$(PULP) repl
.PHONY: repl


test: ## Run tests
	$(PULP) test
.PHONY: test


test-watch: ## Run tests when files change
	$(PULP) --watch test
.PHONY: test-watch
