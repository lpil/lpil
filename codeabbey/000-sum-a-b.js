#! /usr/bin/env node

// var input = '3 5';
var input = '3797420 2064975';

var output = input
          .split(' ')
          .reduce(
              function(acc, e) { return acc + parseInt(e, 10); },
              0);

console.log(output);
