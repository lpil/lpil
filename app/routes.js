'use strict';

var router = require('express').Router();

router.get('/', function(req, res) {
  res.render('root', { content: 'Hi!' });
});

module.exports = router;
