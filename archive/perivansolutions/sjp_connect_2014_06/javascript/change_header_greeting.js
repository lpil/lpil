$(document).ready( function() {
// Remove the last name from the header welcome
// Called from the SiteHeader

  // For some reason the 'Welcome, Firstname Lastname' has a different ID in
  // different parts of the site, so we must cover all of them...

  var id_array = ['ctl00_login_lblWelcome',
    'PageHeader1_login_lblWelcome',
    'ctl01_login_lblWelcome',
    'ctl04_login_lblWelcome'];


  for (var i = 0; i < id_array.length; i++) {
    var elem = document.getElementById(id_array[i]);

    if ( elem !== null ) {
      var welcome = elem.innerHTML;
      welcome = welcome.split(' ');

      // Exit early if there is only one name
      if (welcome.length < 3) { return; }

      // Drop the last word
      var welcome_words = welcome.slice(0,-1);

      // Capitalise the words
      for (var wi = 0; wi < welcome_words.length; wi++) {
        var word = welcome_words[wi];
        word = word.charAt(0).toUpperCase() + word.toLowerCase().slice(1);
        welcome_words[wi] = word;
      }

      // Replace welcome string
      elem.innerHTML = welcome_words.join(' ');
      return;
    }
  }
});
