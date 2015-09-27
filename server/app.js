'use strict';

// Set env
require('./config');

const app    = require('express')();
const routes = require('./routes');

app.set('view engine', 'jade');
app.set('views', './server/views');

// Security middleware
app.use(require('helmet')());

// Log requests
app.use(require('./middlewares/request_logger'));

app.use('/', routes);

// Finally, error handling
app.use(require('./middlewares/error_handler'));

module.exports = app;
