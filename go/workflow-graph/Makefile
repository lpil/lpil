GO_FILES=$(shell find . -type f -name "*.go")

.PHONY: help
help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: test
test: ## Run the tests
	go test ./...

.PHONY: lint
lint: ## Lint the code
	golangci-lint run

.PHONY: build
build: bin/brook ## Compile Brook

.PHONY: fmt
fmt: ## Format the code
	go fmt ./...

.PHONY: test-watch
test-watch: ## Run the Go tests using watchexec
	watchexec -e go "echo && go test ./...; echo"

bin/brook: go.mod go.sum $(GO_FILES)
	go build -o bin/brook cmd/brook/main.go
