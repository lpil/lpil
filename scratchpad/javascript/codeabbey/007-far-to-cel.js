#!/usr/bin/env node

var input, output;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
        .split(' ')
        .slice(1)
        .map(function(num) {
          return parseInt(num, 10);
        });

output = input.map(function(fahrenheit) {
  return Math.round(
      (fahrenheit - 32) * (5/9)
      );
}).join(' ');

console.log(output);
