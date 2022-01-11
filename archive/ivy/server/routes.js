'use strict';

var router = require('express').Router();

router.get('/', (req, res) => {
  res.render('root', { content: 'Hi!' });
});

router.use((req, res) => {
  res.status(404);
  res.render('404');
});

module.exports = router;
