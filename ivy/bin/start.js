'use strict';

const server = require('../server/app');

const mode = process.env.NODE_ENV;
const port = 3000;
server.listen(port, () => {
  console.log(`Server listening on port ${port} in ${mode} mode.`);
});
