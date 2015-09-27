'use strict';

const app    = require('express')();
const routes = require('./routes');

app.set('view engine', 'jade');
app.set('views', './server/views');

// Security middleware
app.use(require('helmet')());

app.use('/', routes);

module.exports = app;
