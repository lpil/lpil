#!/usr/bin/env node

var input, output;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
        .split('\n')
        [1]
        .split(' ')
        .map(function(num) {
          return num.split('')
            .map(function(cha) {
              return parseInt(cha, 10);
            });
        });

output = input.map(function(nums) {
  return nums.reduce(function(acc, num, index) {
    return acc + num * (index + 1);
  });
}).join(' ');

console.log(output);
