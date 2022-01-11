// Display a message on certain pages
var displayPageMessage = function displayPageMessage() {
  var messages = {
    'http://www.sjpconnect.co.uk/UserProductList.aspx?ProductId=17461':
      'This document replaces SJP3393',
  };

  for (var url in messages) {
    if (document.URL === url) {
      var ele = document.createElement('h3');
      ele.innerHTML = messages[url];
      ele.className = 'urlParamMessage';
      var mum = document.getElementById('divCategoryPath').parentNode;
      mum.insertBefore(ele, mum.firstChild);
    }
  }

};

$(document).ready(function() {
  displayPageMessage();
});
