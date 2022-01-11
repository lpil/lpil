BOWER=$(NBIN)/bower

install: ## Install dependancies
	yarn install
	$(BOWER) install
.PHONY: install


build: ## Compile application
	$(PULP) build
.PHONY: build


build-prod: clean output/Hal.js ## Compile and optimise application
.PHONY: build-prod


clean: ## Remove compiled output
	rm -rf output
.PHONY: clean


output/Hal.js:
	$(PULP) build \
		--main Hal \
		--skip-entry-point \
		--optimise \
		--to ./output/Hal.js
	echo 'module.exports = PS.Hal;' >> output/Hal.js
