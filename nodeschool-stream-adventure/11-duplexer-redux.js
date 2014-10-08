#!/usr/bin/env node

var duplexer = require('duplexer'),
    through  = require('through');

module.exports = function(counter) {
  var counts = {},
      write,
      end;

  write = function countCountry(obj) {
    var country = obj.country;

    counts[country] = (counts[country] || 0) + 1;
  };

  end = function setCounts() {
    counter.setCounts(counts);
    counts = {};
  };

  return duplexer(through(write, end), counter);
};
