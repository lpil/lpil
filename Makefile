help:
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

db-console:
	docker run --rm -it --network host orientdb /orientdb/bin/console.sh \
		"CONNECT remote:localhost/particle-test particle-test particle-test"
.PHONY: db-console

db-setup:
	bin/create-orientdb-databases.sh
.PHONY: db-setup
