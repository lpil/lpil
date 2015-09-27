'use strict';

process.env.NODE_ENV = process.env.NODE_ENV || 'development';

switch (process.env.NODE_ENV) {
  case 'production':
    var dbName = 'ivy';
    break;
  case 'test':
    var dbName = 'ivy_test';
    break;
  default:
    var dbName = 'ivy_dev';
}

module.exports = {
  db: {
    name: dbName,
    host: process.env.DB_HOST,
    username: process.env.DB_USER,
    password: process.env.DB_PASSWORD,
  },
};
