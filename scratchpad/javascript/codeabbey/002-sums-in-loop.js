#! /usr/bin/env node

var fs, input, output;

fs = require('fs');
input = fs.readFileSync('./input.txt', 'utf8')
          .split('\n')
          .slice(1, -1)
          .map(function(e) { return e.split(' '); });

output = input.map(function(nums) {
  return nums.reduce(
      function(acc, e) { return acc + parseInt(e, 10); },
      0);
}).join(' ');

console.log(output);
