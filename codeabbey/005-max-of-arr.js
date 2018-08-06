#!/usr/bin/env node

var input, max, min;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
         .split(' ').map(function(num) {
            return parseInt(num, 10);
          });

max = input.reduce(function(acc, num) {
    return (acc < num) ? num : acc;
  });

min = input.reduce(function(acc, num) {
    return (acc > num) ? num : acc;
  });

console.log(max + ' ' + min);
