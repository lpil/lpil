#!/usr/bin/env node

var spawn = require('child_process').spawn;

module.exports = function(cmd, args) {
  var child = spawn(cmd, args);

  return require('duplexer')(child.stdin, child.stdout);
};
