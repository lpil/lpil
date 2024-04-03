#!/usr/bin/env node

require('http').get(process.argv[2], function(response) {
  var datas = [];

  response.setEncoding('utf8');
  response.on('error', console.error);

  response.on('data', function(data) { datas.push(data); });
  response.on('end', function() {
    var out = datas.join('');
    console.log(out.length);
    console.log(out);
  });
});
