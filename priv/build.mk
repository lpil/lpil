install: ## Install dependancies
	yarn install
	$(NBIN)/bower install
.PHONY: install


build: ## Compile application
	$(NBIN)/pulp build
.PHONY: build


build-prod: clean output/Hal.js ## Compile and optimise application
.PHONY: build-prod


clean: ## Remove compiled output
	rm -rf output
.PHONY: clean


output/Hal.js:
	$(NBIN)/pulp build \
		--skip-entry-point \
		--optimise \
		--to ./output/Hal.js
	echo 'module.exports = PS.Main;' >> output/Hal.js
