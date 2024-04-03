calculator: src/*.go
	go build src/calculator.go

build-watch:
	find -name '*.go' | entr go build src/calculator.go
.PHONY: build-watch

test-watch:
	find -name '*.go' | entr go test ./...
.PHONY: test-watch
