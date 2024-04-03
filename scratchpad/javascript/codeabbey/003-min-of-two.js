#! /usr/bin/env node

var input, output;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
        .split('\n')
        .slice(1, -1)
        .map(function(e) { 
          return e.split(' ').map(function(num) {
            return parseInt(num, 10);
          });
        });

output = input.map(function(nums) {
  return (nums[0] < nums[1]) ? nums[0] : nums[1];
}).join(' ');

console.log(output);
