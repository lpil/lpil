'use strict';

const FAQ = require('../../app/models/faq');

describe('database persistance', () => {
  beforeEach((done) => {
    FAQ.truncate().then(() => done());
  });

  it('can insert and select', (done) => {
    const attrs1 = { question: '?',  answer: '!'  };
    const attrs2 = { question: '??', answer: '!!' };
    FAQ.create(attrs1)
    .then(() => FAQ.create(attrs2) )
    .then(() => FAQ.all() )
    .then(faqs => {
      assert.equal(2,    faqs.length);
      assert.equal('?',  faqs[0].question);
      assert.equal('!',  faqs[0].answer);
      assert.equal('??', faqs[1].question);
      assert.equal('!!', faqs[1].answer);
      done();
    });
  });

});
