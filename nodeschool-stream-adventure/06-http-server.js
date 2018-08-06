#!/usr/bin/env node

var http = require('http');

http.createServer(function(req, res) {
      req.setEncoding('utf8');

      req.on('error', console.error);
      req.on('end', function() { res.end(); });

      req.on('data', function(data) {
        if (req.method === 'POST') {
          res.write(data.toUpperCase());
        }
      });
    }
    ).listen(process.argv[2]);
