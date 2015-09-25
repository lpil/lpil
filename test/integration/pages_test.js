'use strict';

var request = require('supertest');
var server  = require('../../app/server');

describe('GET /', () => {
  it('respond with json', (done) => {
    request(server)
      .get('/')
      .expect(200, done);
  });
});

describe('tests', () => {
  it('runs', () => {
    assert.equal(1, 1);
  });
});
