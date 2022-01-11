# Investor Order Lookup

The backend consists of two parts:
* A set of Ruby Rake tasks that allow you to upload a new investor order
  spreadsheet to the db.
* A PHP script through which the user can query the sqlite3 database to find
  out the details of their investor order.

The PHP script can be used in three ways:
* The user can access the PHP script directly, passing their order reference as
  the URL parameter called `partner_code`.
* The PHP script can return the tracking information as a JSON string by
  passing `t` as the URL parameter `json`.
* The PHP script can optionally return the JSON object inside a callback in a
  Javascript string by passing the name of the callback as the URL parameter
  `callback`. This is useful for JSONP cross-site requests. See the front-end
  example files.

## Requirements

* A PHP enabled webserver on a Unix operating system. At time of writing, we
  are using Nginx and PHP-fpm on Debian 7.
* Ruby. At time of writing we are using 2.0.0, managed with RVM.
* Ruby gem dependencies, as specified in the Gemfile. Install by running
  `bundle` from the project directory.

## Installation

1. Provision your PHP capable webserver. A guide on installing a LEMP stack
   (Linux, Nginx, MySQL, PHP) on Debian 7 can be found
   [here](https://www.digitalocean.com/community/tutorials/how-to-install-linux-nginx-mysql-php-lemp-stack-on-debian-7).
   We are using an sqlite3 database, so the MySQL section can be safely
   skipped.
2. Install the php5 sqlite3 adapter.
3. Install [RVM](https://rvm.io/) or similar, along with a suitable version of
   Ruby.
4. Clone this repository on the server, and configure the Nginx server
   appropriately. An existing config can be found in another repo.
5. Install the Ruby dependancies by running `bundle install` from the project
   directory.
6. **TODO- setup Nginx permissions for this dir**
7. From the project directory run `bundle exec rake db:migrate` to create the
   sqlite3 database.
8. **TODO- the rest**

## Uploading a new spreadsheet to the DB

The spreadsheet should be in CSV format.

From the project directory:

    bundle exec rake investor:upload spreadsheet=path/to/spreadsheet.csv
