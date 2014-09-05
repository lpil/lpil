var trackUrl =
  'http://info.perivansolutions.co.uk/order-tracking/results.php';

// IE sucks hard
if (!window.console) {
  window.console = { log: function() {} };
}

var createScriptTag = function createScriptTag(source) {
  var head= document.getElementsByTagName('head')[0];
  var script = document.createElement('script');
  script.type = 'text/javascript';
  script.src = source;
  head.appendChild(script);
};

// Cross site JSON request (JSONP)
var getMailInfo = function getMailInfo(order_ref) {
  // Clear any existing results
  var result_div = document.getElementById('js-mail_result');
  result_div.innerHTML = '';

  // Display help message
  var link = document.createElement('a');
  link.href = 'http://info.perivansolutions.co.uk/delivery';
  link.target = '_blank';
  link.innerHTML = 'Slow response? Try here instead';
  var spinner = document.createElement('img');
  spinner.src = 'Images/AjaxWait.gif';

  result_div.appendChild(spinner);
  result_div.appendChild(document.createElement('br'));
  result_div.appendChild(link);

  // Send request
  console.log('Querying Perivan for order_ref: ' + order_ref);
  var url = trackUrl + '?json=t&order_ref=';
  url = url + order_ref + '&callback=printMailInfo';
  createScriptTag(url);

  // Prevent the form from submitting
  return false;
};

var printMailInfo = function printMailInfo(jsonp){
  console.log('Reply from Perivan: ' + jsonp);

  var result_div = document.getElementById('js-mail_result');
  result_div.innerHTML = '';

  var para = document.createElement('p');
  if (jsonp === null) {
    para.innerHTML = 'No record found for this order reference';
    result_div.appendChild(para);

  } else if (jsonp.is_post) {
    para.innerHTML = 'Your order was sent by post on ' + jsonp.date_sent;
    result_div.appendChild(para);

  } else {
    para.innerHTML = 'Your order was sent by DPD on ' + jsonp.date_sent;
    result_div.appendChild(para);

    var link = document.createElement('a');
    link.href = jsonp.url;
    link.target = '_blank';
    link.innerHTML = 'Track on DPD.co.uk';
    result_div.appendChild(link);
  }
};
