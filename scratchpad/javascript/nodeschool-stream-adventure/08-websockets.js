var stream = require('websocket-stream')('ws://localhost:8000');

stream.end('hello\n');
