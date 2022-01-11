#!/usr/bin/env node

var input, output;

input = require('fs')
        .readFileSync('./input.txt', 'utf8')
        .split('\n')
        .slice(1, -1);

output = input
          .map(function(string) {
            return string.split('')
              .filter(function(cha) {
                return /[aeiouy]/.test(cha);
              }).length;
          })
          .join(' ');

console.log(output);
