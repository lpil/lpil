#!/usr/bin/env node

var url = require('url'),
    routes;

routes = {
  '/api/parsetime': function(parsedUrl) {
    var date = new Date(parsedUrl.query.iso);
    return {
      'hour':   date.getHours(),
      'minute': date.getMinutes(),
      'second': date.getSeconds()
    };
  },

  '/api/unixtime': function(parsedUrl) {
    return {
      unixtime: (new Date(parsedUrl.query.iso)).getTime()
    };
  }
};

require('http')
  .createServer(function(request, response) {
    var parsedUrl = url.parse(request.url, true),
        route = routes[parsedUrl.pathname];

    if (route) {
      response.writeHead(200, { 'Content-Type': 'application/json' });
      response.end(
          JSON.stringify(route(parsedUrl))
          );
    } else {
      response.writeHead(404);
      response.end();
    }
  })
  .listen(process.argv[2]);
