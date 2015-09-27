server:
	./node_modules/nodemon/bin/nodemon.js bin/start.js

test:
	./node_modules/mocha/bin/mocha --watch

migrate:
	./node_modules/sequelize-cli/bin/sequelize db:migrate

rollback:
	./node_modules/sequelize-cli/bin/sequelize db:migrate:undo

.PHONY:    \
	test     \
	server   \
	migrate  \
	rollback \
