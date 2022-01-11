// Redirect to another page if certain things have been searched for
var searchRedirect = function searchRedirect() {
  var redirects = {
    'sjp3393': 'UserProductList.aspx?ProductId=17461',
    '3393':    'UserProductList.aspx?ProductId=17461'
  };
  var searchStr;
  var catPathText = document.getElementById(
      'divCategoryPath').innerHTML.toLowerCase();

  for (var search in redirects) {
    searchStr = 'search results for "' + search + '"';
    if (searchStr === catPathText) {
      // Clear search page message if it exists
      var x = document.getElementById('pnlHide');
      if (x) { x.innerHTML = ''; }
      // Redirect
      window.location.replace( redirects[search] );
      return true;
    }
  }
};

$(document).ready(function() {
  searchRedirect();
});
