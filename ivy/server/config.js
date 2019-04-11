'use strict';

process.env.NODE_ENV = process.env.NODE_ENV || 'development';
if (process.env.NODE_ENV !== 'production') {
  require('dotenv').load();
}

module.exports = {
  production: {
    database: 'ivy',
    host: process.env.DB_HOST,
    username: process.env.DB_USER,
    password: process.env.DB_PASSWORD,
    dialect: 'postgres',
  },
  development: {
    database: 'ivy_dev',
    host: process.env.DB_HOST,
    username: process.env.DB_USER,
    password: process.env.DB_PASSWORD,
    dialect: 'postgres',
  },
  test: {
    database: 'ivy_test',
    host: process.env.DB_HOST,
    username: process.env.DB_USER,
    password: process.env.DB_PASSWORD,
    dialect: 'postgres',
  },
};
