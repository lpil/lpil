#!/usr/bin/env node

console.log(
    require('fs')
      .readFileSync(process.argv[2], 'utf8')
      .split('\n')
      .length - 1
    );
