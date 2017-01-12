help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


#
# We sleep to avoid race conditions with crystal formatter :(
#
test-watch: ## Automatically run tests when a file is saved
	crystal spec || exit 0
	while inotifywait -e CLOSE_WRITE -rq .;\
	do\
		echo -e "\n\n\n\n\n";\
		sleep 0.1;\
		crystal spec;\
	done

.PHONY: test-watch
