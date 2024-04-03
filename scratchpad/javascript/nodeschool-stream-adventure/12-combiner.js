#!/usr/bin/env node

var split    = require('split'),
    through  = require('through'),
    zlib     = require('zlib'),
    combiner = require('stream-combiner'),
    genreCounter,
    currentGenre, 
    write,
    end;

write = function write(line) {
  var data;

  if (line.length > 0) {
    data = JSON.parse(line.toString());

    if (data.type === 'genre') {
      if (currentGenre) {
        this.queue(JSON.stringify(currentGenre) + '\n');
      }

      currentGenre = { name: data.name, books: [] };
    }
    else {
      currentGenre.books.push(data.name);
    }
  }
};

end = function end() {
  if (currentGenre) {
    this.queue(JSON.stringify(currentGenre) + '\n');
  }
  else {
    this.queue(null);
  }
};

genreCounter = through(write, end);

module.exports = function () {
  return combiner(
      split(),
      genreCounter,
      zlib.createGzip()
      );
};
