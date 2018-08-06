#!/usr/bin/env node

var http = require('http'),
    urls = process.argv.slice(2),
    bodies = [],
    incomplete = urls.length;

var getAndRecord = function getAndRecord(url, index) {
  http.get(url, function(response) {
    bodies[index] = '';

    response.setEncoding('utf8');
    response.on('error', console.error);

    response.on('data', function(data) {
      bodies[index] += data;
    });

    response.on('end', function() {
      incomplete -= 1;

      if (incomplete === 0) { console.log(bodies.join('\n')); }
    });
  });
};

for (var i = 0, l = urls.length; i < l; i ++) {
  getAndRecord(urls[i], i);
}
