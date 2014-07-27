# AXA Marketing Store as a Ruby on Rails application

## Features

* Collections
  * ~~Application is divided into Collections. One per client.~~
  * Members of collections cannot view the data of other collections
  * Collections can have their own site themes

* Categories
  * Categories can contain documents
  * Categories can be nested
  * Categories can be added and removed

* Documents
  * Have metadata fields that can be set per collection
  * Have an expiry date, after which they are to be removed automatically
  * Can be uploaded by users
    * with permission to
  * Can be searched for
  * PDF docs have a thumbnail image
  * Documents can be downloaded (downloads recorded)
  * Documents can be emailed (emails recorded)

* Users
  * ~~Users can be created~~
  * ~~Users can be removed~~
  * ~~Users can change their passwords~~
  * ~~Users can change their emails~~
  * ~~Users can either have upload permissions or not~~
  * ~~Users can either have reporter permissions or not~~

* Admins
  * Can edit documents
  * Can delete documents
  * Can create new users
  * Can edit user details
  * Can change user passwords (email it to them?)

* Reports
  * Reports can be accessed by users with permissions
  * A report of all documents can be generated
    * Grouped by expiry date (expired, soon to expire, etc)
    * Spreadsheet format
    * HTML format?
  * A report of all users can be generated

* Front end
  * ~~Site should be mobile device friendly~~
  * ~~Use CSS framework such as Foundation~~

* Security
  * HTTPS should probably be used

## Notes

### Storing the category tree in the db

    # model
    class Category < ActiveRecord::Base
      has_many :children, class_name: 'Category',
        foreign_key: 'parent_id'

      belongs_to :parent, class_name: 'Category'
    end

      # Migration
        class CreateCategories < ActiveRecord::Migration
      def change
        create_table :categories do |t|
          t.string :name
          t.references :parent

          t.timestamps
        end
      end
    end

      # De-serialise the tree
    def get_tree(node)
      return node.attributes if node.children.empty?
      hash = node.attributes
      hash[:children] = node.children.map { |n| get_tree(n) }
      hash
    end

Better idea- query the entire table, convert it to json, and rebuild the tree
client side, using coffeescript

### Document upload + storage

* Use Rackspace Cloud files for storage
  * `https://github.com/carrierwaveuploader/carrierwave`
* Turn on and off worker processes as needed
  * `http://ctoinsights.wordpress.com/2011/11/26/running-heroku-workers-only-when-required/`
* Or run them on the same dyno
  * `http://stackoverflow.com/questions/18176043/does-anyone-run-more-than-one-resque-worker-in-a-heroku-dyno/19764369#19764369`
  * `http://stackoverflow.com/questions/21566721/parallel-background-tasks-on-single-worker-dyno`

### Document Expiry

* Periodic task using Heroku scheduler
  * `https://devcenter.heroku.com/articles/scheduler`
* Or periodic code block using sleep with date maths
  * `http://stackoverflow.com/questions/19448091/ruby-sleep-to-specific-time`

### Document metadata

* `http://railscasts.com/episodes/302-in-place-editing`

### Database

* `http://railscasts.com/episodes/342-migrating-to-postgresql?view=asciicast`

### Database backup

* `https://addons.heroku.com/pgbackups#auto-month`

### Searching

* `http://texticle.github.io/texticle/`

### Exception monitoring

* `https://addons.heroku.com/rollbar#free`
* `http://railscasts.com/episodes/402-better-errors-railspanel`

### Security

* `http://guides.rubyonrails.org/security.html`
* `http://railscasts.com/episodes/358-brakeman`

### Admin

* `http://railscasts.com/episodes/284-active-admin?autoplay=true`
