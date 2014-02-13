pageflex_product_report
=======================

Generate a CSV of products from an XML dump of a Pageflex Storefront database

# Notes

The columns included in the report is dictated by the 'columns' array variable.
This version uses our custom metadata fields, so it will likely fail if you run
it without removing these values from the columns array.  
These values are...

    ['Document Type', :"Document Type"],
    ['Team', :Team],
    ['Contact', :Contact],
    ['Key Words', :Keywords__STR],

# Dependencies

* **nokogiri** - for parsing XML
