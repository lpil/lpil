(function() {
  'use strict';

  var elem, count, fields, update, p, i;

  fields = [
    ['#FIELD_87', 10],
    ['#FIELD_668', 100]
  ];

  update = function update(el, charsLeft) {
    el.textContent = charsLeft + ' characters remaining';
    el.style.color = charsLeft < 0 ? '#f00' : '#000';
  };

  count = function count(el, maxChars) {
    var charsLeft = maxChars - el.value.length;
    update(el.nextSibling, charsLeft);
  };

  i = fields.length;
  while (i--) {
    elem = document.querySelector(fields[i][0]),
           p = document.createElement('p');

    elem.parentElement.appendChild(p);

    update(p, fields[i][1] - elem.value.length);

    elem.addEventListener(
          'input',
          (function(ii) {
            return function() { count(this, fields[ii][1]); };
          }(i)));
  }
}());
