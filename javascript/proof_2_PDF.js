$(document).ready( function() {
// Change "Proof" to "PDF" on product list
// Called from the SiteHeader

  var unit_inputs = $("#roiProdList_gridViewProducts > tbody:nth-child(1) > tr > td:nth-child(7) > a").get();

  for (var i = 0; i < unit_inputs.length; i++) {
    unit_inputs[i].innerHTML = "PDF";
  }
});
