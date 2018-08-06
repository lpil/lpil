#!/usr/bin/env node

// main

require('./05-z-module')(process.argv[2],
                         process.argv[3],
                         function(err, data) {
  if (err) { throw err; }
  console.log(data.join('\n'));
});
