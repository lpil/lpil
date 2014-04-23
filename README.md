pageflex_product_report
=======================

Generate a CSV of products from an XML dump of a Pageflex Storefront 8 database

# Notes

The columns included in the report is dictated by the 'columns' array variable.
This version uses our custom metadata fields, you may want to remove these from
the columns array.  
These values are...

    ['Expiry Date', :"metadata_Expiry Date"],
    ['Document Type', :"metadata_Document Type"],
    ['Team', :metadata_Team],
    ['Contact', :metadata_Contact],

There are also some other funky non-generic features that you may want to
change, such as the sort_by_uk_date and strip_html methods.

# Dependencies

* **nokogiri** - for parsing XML
