'use strict';

var router = require('express').Router();

router.get('/', function(req, res) {
  res.send('Hello, world!');
});

module.exports = router;
