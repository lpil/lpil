#!/usr/bin/env node

var split = require('split'),
    through = require('through'),
    upcaseSwitch = false,
    evenUpcaseFilter;

evenUpcaseFilter = through(function(line) {
    line = line.toString();

    if (upcaseSwitch) {
      line = line.toUpperCase();
    } else {
      line = line.toLowerCase();
    }
    upcaseSwitch = !upcaseSwitch;
    this.queue(line + '\n');
  });


process.stdin
  .pipe(split())
  .pipe(evenUpcaseFilter)
  .pipe(process.stdout);
