'use strict';

const config = require('./config');

const Sequelize = require('sequelize');
const env = process.env.NODE_ENV;

const connection = new Sequelize(
  config[env].database,
  config[env].username,
  config[env].password,
  {
    host: config[env].host,
    dialect: config[env].dialect,
    logging: false,

    pool: {
      max: 5,
      min: 0,
      idle: 10000,
    },
  }
);

module.exports = { Sequelize, connection };
