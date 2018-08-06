#!/usr/bin/env node

var input, output;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
        .split('\n')
        .slice(1, -1)
        .map(function(nums) {
          return nums.split(' ')
            .map(function(num) {
              return parseFloat(num, 10);
            });
        });

output = input.map(function(nums) {
  var bmi = nums[0] / Math.pow(nums[1], 2);

  if      (bmi < 18.5) { return 'under';  }
  else if (bmi < 25)   { return 'normal'; }
  else if (bmi < 30)   { return 'over';   }
  else                 { return 'obese';  }

}).join(' ');

console.log(output);
