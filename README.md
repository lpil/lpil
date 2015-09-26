Ivy
===

```sh
# Install JS deps
npm install
# Install and set up Postgres database
brew install postgresql
pg_ctl init
createdb
pg_ctl start
psql -c "CREATE DATABASE ivy_dev;"
psql -c "CREATE DATABASE ivy_test;"

# Start test watcher
npm test
# Or only run certain tests
npm test --grep test/integration

# Run server
npm start
```

## Deps

Here are some libraries we're using that you may want to know more about.

### Web

* [Express][express], web micro-framework.
* [Jade][jade], speedy templating language.
* [Sequelize][sequelize], database ORM.

[express]: https://github.com/strongloop/express
[jade]: https://github.com/jadejs/jade
[sequelize]: https://github.com/sequelize/sequelize

### Tests

* [Mocha][mocha], test framework.
* [supertest][supertest], HTTP integration test tool.

[supertest]: https://github.com/visionmedia/supertest
[mocha]: https://github.com/mochajs/mocha


## Notes

Handy reading.

* [Unit testing controllers][controller-test]

[controller-test]: http://www.designsuperbuild.com/blog/unit_testing_controllers_in_express/


## Reference applications

These look pretty good. Let's study them when we're not sure what the
idiomatic way to solve a problem is.

* [Ghost blogging platform][ghost]
* [Hackathon Starter][hackathon-starter]

[ghost]: https://github.com/tryghost/Ghost
[hackathon-starter]: https://github.com/sahat/hackathon-starter
