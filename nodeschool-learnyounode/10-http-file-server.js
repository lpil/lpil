#!/usr/bin/env node

var fs = require('fs');

require('http')
  .createServer(function(request, response) {
    fs.createReadStream(process.argv[3])
      .pipe(response);
  })
  .listen(process.argv[2]);
