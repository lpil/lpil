#!/usr/bin/env node

var trumpet = require('trumpet'),
    tr      = trumpet(),
    through = require('through'),
    loud;

loud = tr.select('.loud').createStream();

loud.pipe(through(function(data) {
    this.queue(data.toString().toUpperCase());
  }))
  .pipe(loud);

process.stdin.pipe(tr).pipe(process.stdout);
