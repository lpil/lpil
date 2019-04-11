'use strict';

// A middleware that logs requests.
//
// Nothing is logged in test env
// A little is logged in the development env
// A lot of logged in other envs

var logger;

switch (process.env.NODE_ENV) {
  case 'test':
    logger = require('./noop');
    break;

  case 'development':
    logger = require('morgan')('dev');
    break;

  default:
    logger = require('morgan')('combined');
}

module.exports = logger;
