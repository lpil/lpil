pageflex_product_report
=======================

Generate a CSV of products from an XML dump of a Pageflex Storefront 8 database

# Instructions

1. Export the database to an XML file (Administration > Database Tools in the
   administrator side of the site)
2. Run `bin/report.rb path/to/database_dump.xml` :)

# Notes

The other executables in `bin/` are customised reports that may have additional
dependancies or specific product metadata.
