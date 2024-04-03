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
          }).slice(0, -1);
        });

output = input.map(function(nums) {
  return Math.round(
      (nums.reduce(function(acc, e) { return acc + e; })) / nums.length
      );
}).join(' ');

console.log(output);
