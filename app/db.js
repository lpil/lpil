'use strict';

const Sequelize = require('sequelize');

function dbName() {
  switch (process.env.NODE_ENV) {
    case 'prod':
      return 'ivy';
    case 'test':
      return 'ivy_test';
    default:
      return 'ivy_dev';
  }
}


const connection = new Sequelize(dbName(), 'postgres', 'postgres', {
  host: 'localhost',
  dialect: 'postgres',
  logging: false,

  pool: {
    max: 5,
    min: 0,
    idle: 10000
  },
});

module.exports = { Sequelize, connection };
