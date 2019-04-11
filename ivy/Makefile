clean:
	rm -r node_modules public

server:
	./node_modules/nodemon/bin/nodemon.js bin/start.js

test:
	./node_modules/mocha/bin/mocha --watch

frontend:
	./node_modules/brunch/bin/brunch watch

frontend-production:
	./node_modules/brunch/bin/brunch build --production

migrate:
	./node_modules/sequelize-cli/bin/sequelize db:migrate

rollback:
	./node_modules/sequelize-cli/bin/sequelize db:migrate:undo

.PHONY: \
	clean               \
	test                \
	server              \
	migrate             \
	rollback            \
	frontend            \
	frontend-production \
