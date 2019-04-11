'use strict';

var request = require('supertest');
var server  = require('../../server/app');

describe('GET /', () => {
  it('respond', (done) => {
    request(server)
      .get('/')
      .expect(200, done);
  });
});
