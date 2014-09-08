# Order Tracking

The backend consists of two parts:
* A set of Ruby Rake tasks that periodically add new order reports to the
  sqlite3 database, and remove old reports.
* A PHP script through which the user can query the sqlite3 database to find
  out the delivery details of their order.

The PHP script can be used in three ways:
* The user can access the PHP script directly, passing their order reference as
  the URL parameter called `order_ref`.
* The PHP script can return the tracking information as a JSON string by
  passing `t` as the URL paramter `json`.
* The PHP script can optionally return the JSON object inside a callback in a
  Javascript string by passing the name of the callback as the URL parameter
  `callback`. This is useful for JSONP cross-site requests. See the front-end
  example files.

## Requirements

* A PHP enabled webserver on a Unix operating system. At time of writing, we
  are using nginx and PHP-fpm on Debian 7.
* Ruby. At time of writing we are using 2.0.0, managed with RVM.
* Ruby gem dependencies, as specified in the Gemfile. Install by running
  `bundle` from the project directory.
