$(document).ready(function(){
  // find text link in sidebar containing specified text, then hide parent
  // table cell
  $("a.categorySidebarLabel:contains('The Investor')").parent().hide();
});
