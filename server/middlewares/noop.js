'use strict';

// A middleware that does nothing!

module.exports = function(_req, _res, next) {
  next();
};
