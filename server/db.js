'use strict';

const config = require('../config/config');

const Sequelize = require('sequelize');

const connection = new Sequelize(
  config.db.name,
  config.db.username,
  config.db.password,
  {
    host: config.db.host,
    dialect: 'postgres',
    logging: false,

    pool: {
      max: 5,
      min: 0,
      idle: 10000,
    },
  }
);

module.exports = { Sequelize, connection };
