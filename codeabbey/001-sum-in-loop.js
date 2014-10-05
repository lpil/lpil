#! /usr/bin/env node

var fs, input, output;

fs = require('fs');
input = fs.readFileSync('./input.txt', 'utf8')
          .split('\n')
          .slice(1, -1)
          [0]
          .split(' ');


output = input.reduce(
  function(acc, e) { return acc + parseInt(e, 10); },
  0);

console.log(output);
