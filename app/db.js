'use strict';

const Sequelize = require('sequelize');

const connection = new Sequelize(
  process.env.DB_NAME,
  process.env.DB_USER,
  process.env.DB_PASSWORD,
  {
    host: process.env.DB_HOST,
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
