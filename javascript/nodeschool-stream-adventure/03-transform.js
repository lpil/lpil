#!/usr/bin/env node

process.stdin
  .on('data', function(data) {
    process.stdout
      .write(data.toString().toUpperCase());
  });

// Or with the through module

// var thruFilter = require('through')(function(buf) {
//   this.queue(buf.toString().toUpperCase());
// });
// process.stdin
//   .pipe(thruFilter)
//   .pipe(process.stdout);
