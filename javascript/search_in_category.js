$('document').ready(function() {
  // Exit if we're not on a product list category
  if (document.getElementById('roiProdList_gridViewProducts') === null) {
    return;
  }
  // Insert search in category button
  var button = document.createElement('a');
  button.innerHTML = 'Search in category';
  button.className = 'searchButton';
  button.id = 'btnSearchCat';
  button.href = 'javascript:slideSearchPopup(0,90);searchInCat();void(0)';
  document.querySelector('.searchButton-inner').appendChild(button);
});

function slideSearchPopup(fromHeight, toHeight) {
  var popup = document.getElementById('search_in_cat_popup');
  currentHeight = fromHeight;
  popup.style.height = currentHeight;

  var round;
  if (fromHeight < toHeight) {
    round = function(num) { return Math.ceil(num); };
  } else {
    round = function(num) { return Math.floor(num); };
  }

  var interval = setInterval(function() {
    if (currentHeight === toHeight) {
      clearInterval(interval);
    } else {
      currentHeight += round((toHeight - currentHeight) / 20);
      popup.style.height = currentHeight + 'px';
    }
  }, 10);
}

function searchInCat() {
  var searchTerm = document.getElementById('txtSearch').value;
  var prodSelector = '#roiProdList_gridViewProducts > tbody > tr';

  // Use the array slice method to drop the first row (header) from nodelist
  var products = [].slice.call(document.querySelectorAll(prodSelector), 1);
  searchTerm = searchTerm.toLowerCase();

  var clear_search_classes = function clear_search_classes() {
    // Remove result class from other products
    for (var x = 0; x < products.length; x++) {
      products[x].className =
        products[x].className.replace(
            /( |^)search_in_cat_result( |$)/ , '');
    }
  };

  var cat_search = function cat_search(searchTerm, start) {
    for (var i = start; i < products.length; i++) {

      // If product contains search string
      // AND does not already have the class
      if (products[i].textContent.toLowerCase().indexOf( searchTerm ) !== -1 &&
          products[i].className.indexOf( 'search_in_cat_result' ) == -1) {

            clear_search_classes();

            // Add result class to this one
            products[i].className =
              products[i].className + ' search_in_cat_result';

            // Scroll into view
            products[i].previousSibling.scrollIntoView();

            return i;
          }
    }
    // clear_search_classes();
    return -1;
  };

  var newStart = cat_search(searchTerm, searchInCatStart + 1);

  // If we reached the end of the products, and we didn't start searching from
  // the beginning, search again from the beginning
  if (newStart === -1 && searchInCatStart !== -1) {
    newStart = cat_search(searchTerm, 0);
  }
  searchInCatStart = newStart;
}
var searchInCatStart = -1;
