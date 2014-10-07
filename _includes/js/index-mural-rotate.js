(function() {
  var texts = document.getElementsByClassName('js-index__mural__text');

  window.setInterval(function() {
    texts[0].parentNode.insertBefore(
        texts[0], texts[texts.length - 1].nextSibling);
  }, 8000);
}());
