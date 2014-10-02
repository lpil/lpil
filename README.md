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
5. Set up your webserver not to serve anything but HTML and CSS files from the
   project directory, so that users cannot download the code or database. See
   below for an example.
6. From the project directory run `bundle exec rake db:migrate` to create the
   sqlite3 database.
7. Make a yaml file at `config/ftp.yml` with the details of the FTP site to
   read the reports from. You can find an example at `config/ftp.yml.example`.
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

# Nginx config

Here is an example server block for nginx

    server {
      server_name www.perivansolutions.co.uk perivansolutions.co.uk;

      root /usr/share/nginx/www/perivansolutions/;
      index index.html index.htm;

      location ~ /order-tracking {
        root /usr/share/nginx/www/;

        # Whitelist. Serve these files extensions only
        if ($request_filename !~* \.(css|js|php|html)$ ) {
          return 403;
          break;
        }

        location ~ \.php$ {
          try_files $uri =404;
          fastcgi_pass unix:/var/run/php5-fpm.sock;
          fastcgi_index index.php;
          fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
          include fastcgi_params;
        }
      }

      location / {
        try_files $uri $uri/ index.html;
      }
    }

For more examples see [this
repo](https://github.com/PerivanSolutions/nginx.conf)
