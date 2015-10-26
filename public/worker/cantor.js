'use strict';

// 1.0
// 1.0         0.5          1.0
// 1.0 0.5 1.0 0.5 0.25 0.5 1.0 0.5 1.0

function mult(arr, n) {
  return arr.map(function(x) { return x * n; });
}

function vecMult(xs, ys) {
  return xs.reduce(function(acc, x) {
    return acc.concat(mult(ys, x));
  }, []);
}

function cantor(pat, iterations) {
  var results = [],
      arr     = [1],
      maxLen  = Math.pow(2, 19),
      maxSeedLen;
  iterations = iterations || 10;
  maxSeedLen = Math.ceil(maxLen / pat.length);

  while (iterations--) {
    arr = arr.slice(0, maxSeedLen);
    arr = vecMult(pat, arr);
    results.push(arr.slice(0, maxLen));
  }
  return results;
}

onmessage = function(args) {
  var pattern    = args.data[0] || [0.33,1,0.75,0.5],
      iterations = args.data[1] || 7,
      data       = cantor(pattern, iterations);
  postMessage(data);
  close();
};
