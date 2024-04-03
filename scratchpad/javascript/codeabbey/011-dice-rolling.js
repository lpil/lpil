#!/usr/bin/env node

var input, output;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
        .split('\n')
        .slice(1, -1)
        .map(function(num) {
          return parseFloat(num);
        });

output = input.map(function(num) {
  return Math.floor(num * 6) + 1;
}).join(' ');

console.log(output);
