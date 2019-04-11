'use strict';

var request = require('supertest');
var server  = require('../../server/app');

describe('an unknown path', () => {
  it('respond with a 404 page', (done) => {
    request(server)
    .get('/some/unknown/path')
    .expect(404)
    .expect(/404: Page not found/, done);
  });
});
