#!/usr/bin/env node

console.log(
    process.argv
      .slice(2)
      .reduce(function(acc, e) {
        return acc + parseInt(e, 10);
      }, 0)
    );
