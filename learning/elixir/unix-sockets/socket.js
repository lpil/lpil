const net = require('net');
const server = net.createServer((c) => {
  // 'connection' listener
  console.log('client connected');
  c.on('end', () => {
    console.log('client disconnected');
  });
  c.on('data', data => {
    console.log(data.toString('utf8'));
    c.write('Hello, from JS');
  });
});
server.on('error', (err) => {
  throw err;
});
server.listen('/tmp/my-socket', () => {
  console.log('server bound');
});
