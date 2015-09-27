'use strict';

const db = require('../db');

const FAQ = db.connection.define('faq', {
  question: db.Sequelize.TEXT,
  answer:   db.Sequelize.TEXT,
});

FAQ.sync();

module.exports = FAQ;
