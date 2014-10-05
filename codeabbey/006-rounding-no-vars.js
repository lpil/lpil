#!/usr/bin/env node

// And as a one liner, just for fun

console.log(
  require('fs')
    .readFileSync('./input.txt', 'utf8')
    .split('\n')
    .slice(1, -1)
    .map(function(nums) {
      return nums.split(' ')
            .map(function(num) {
              return parseInt(num, 10);
            });})
    .map(function(nums) {
      return Math.round(nums[0] / nums[1]);
    })
    .join(' '));
