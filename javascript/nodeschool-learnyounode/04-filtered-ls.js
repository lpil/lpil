#!/usr/bin/env node

require('fs').readdir(process.argv[2], function(err, files) {
  if (err) { throw err; }
  console.log(
      files.filter(function(file) {
        return require('path').extname(file) === '.' + process.argv[3];
      }
      ).join('\n'));
});
