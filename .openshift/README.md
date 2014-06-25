# Openshift PaaS

Openshift is an open source platform-as-a-service from Red Hat. Hooray!

## Deploying for the first time.

I made the sqlite3 db locally, and copied it to the openshift data directory
using scp.

A deploy action hook was added to bundle exec rake db:migrate.

The production db location was changed to use the `OPENSHIFT_DATA_DIR`
