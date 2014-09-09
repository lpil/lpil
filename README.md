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
2. Install [RVM](https://rvm.io/) or similar, along with a suitable version of
   Ruby.
3. Clone this repository into the Nginx webroot directory, or another directory
   Nginx will serve content from.
4. Install the Ruby dependancies by running `bundle install` from the project
   directory.
5. **TODO- setup Nginx permissions for this dir**
6. From the project directory run `bundle exec rake db:migrate` to create the
   sqlite3 database.
7. **TODO- setup FTP passwords**
8. Populate the database with reports for the first time by running the
   following:
   * `bundle exec rake orders:clean`
   * `bundle exec rake orders:fetch`
   * `bundle exec rake orders:parse`
9. Now we need to use [Cron](https://en.wikipedia.org/wiki/Cron) to
   periodically add new reports and to remove old ones. We shall do this
   indirectly using the `whenever` Ruby gem, rather than by directly editing
   the crontab. From the project directory run `bundle exec whenever -w`.
   Ensure that the crontab has been written to with `crontab -l`.
