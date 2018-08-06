#!/usr/bin/env node

var input, output;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
        .split('\n')
        .slice(1, -1)
        .map(function(nums) {
          return nums.split(' ')
            .map(function(num) {
              return parseInt(num, 10);
            });
        });

output = input.map(function(nums) {
  return nums.sort(function(a,b) {
    return a - b;
  })[1];
}).join(' ');

console.log(output);
