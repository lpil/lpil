'use strict';

module.exports = function(err, req, res, _next) {
  res.status(err.status || 500);

  if (process.env.NODE_ENV === 'development') {
    // Print error in development
    console.log(`Error: ${err.message}`);
  } else {
    // Don't leak error messages in prod
    err = {};
  }

  res.render(
    'error',
    {
      message: err.message,
      error: err,
    }
  );
};
