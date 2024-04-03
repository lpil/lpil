#!/usr/bin/env node

var request = require('request');

process.stdin
  .pipe(request.post('http://localhost:8000'))
  .pipe(process.stdout);
