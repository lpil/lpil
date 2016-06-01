(function(window) {
  var href = window.location.href;
  if (href.match(/^http:\/\/lpil.uk/)) {
    var dest = href.replace(/^http/, "https");
    window.location = dest;
  }

  // Email addresses in CSS have been muddled for anti-spam reasons.
  var unmuddle = function unmuddle(email) {
    email.href = email.href.replace(/my-first-name/, 'louis');
    email.href = email.href.replace(/this-domain/, 'lpil.uk');
  };
  var muddled_emails = document.getElementsByClassName('js-muddled-email');
  for (var i = 0, l = muddled_emails.length; i < l; i ++) {
    unmuddle(muddled_emails[i]);
  }
}(window));
