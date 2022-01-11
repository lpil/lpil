$(document).ready( function() {
// Re-Link the Home tab to the Home category
// Called from the SiteHeader

  // For some reason the home button has a different ID in different parts
  // of the site, so we must cover all of them.

  var id_array = ['PageHeader1_start_anchor',
    'PageHeader_start_anchor',
    'pageHeaderControl_start_anchor',
    'ctl00_start_anchor',
    'ctl01_start_anchor',
    'ctl04_start_anchor'];

  for (var i = 0; i < id_array.length; i++) {
    var link = document.getElementById(id_array[i]);
    if (link !== null) {
      var link_att = link.attributes;
      link_att.getNamedItem('href').value = 'UserContentStart.aspx?category=429';
      return
    }
  }
});
