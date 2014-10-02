(function() {
  'use strict';
  var trackUrl =
    'http://info.perivansolutions.co.uk/order-tracking/results.php';

  // Cross site JSON request (JSONP)
  var getMailInfo = function getMailInfo(order_ref) {
    var result_div,
        link,
        spinner,
        script;

    // Clear any existing results
    result_div = document.getElementById('js-mail_result');
    result_div.innerHTML = '';

    // Display help message
    link = document.createElement('a');
    link.href = 'http://info.perivansolutions.co.uk/delivery';
    link.target = '_blank';
    link.innerHTML = 'Slow response? Try here instead';
    spinner = document.createElement('img');
    spinner.src = 'Images/AjaxWait.gif';

    result_div.appendChild(spinner);
    result_div.appendChild(document.createElement('br'));
    result_div.appendChild(link);

    // Send request
    script = document.createElement('script');
    script.type = 'text/javascript';
    script.src =
      trackUrl + '?json=t&order_ref=' + order_ref + '&callback=printMailInfo';
    document.getElementsByTagName('head')[0].appendChild(script);

    return false;
  };


  var printMailInfo = function printMailInfo(jsonp){
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

  // Expose required functions to global namespace
  window.getMailInfo   = getMailInfo;
  window.printMailInfo = printMailInfo;
}());
