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
  return (nums[0] * nums[1] + nums[2])
    .toString()
    .split('')
    .reduce(function(acc, e) {
      return acc + parseInt(e, 10);
    }, 0);
}).join(' ');

console.log(output);
