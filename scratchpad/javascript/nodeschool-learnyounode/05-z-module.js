#!/usr/bin/env node

// module

module.exports = function(dir, fileExt, callback) {
  require('fs').readdir(dir, function(err, files) {
    if (err) { return callback(err); }

    var filtered = files.filter(function(file) {
          return require('path').extname(file) === '.' + fileExt; });

    return callback(err, filtered);
  });
};
