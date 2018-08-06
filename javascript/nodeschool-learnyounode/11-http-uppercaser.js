#!/usr/bin/env node

// I'm a strong independent code monkey, I don't need no through2-map library

require('http')
  .createServer(function(request, response) {
    request.on('error', console.error);
    request.on('end', function() { response.end(); });

    if (request.method === 'POST') {
      request.setEncoding('utf8');

      request.on('data', function(data) {
        console.log(data);
        response.write(data.toUpperCase());
      });
    }
  })
  .listen(process.argv[2]);
