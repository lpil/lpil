'use strict';

exports.config = {
  files: {
    javascripts: {
      joinTo: 'public/app.js'
    },
    templates: {
      joinTo: 'public/app.js'
    },
    stylesheets: {
      joinTo: 'public/app.css'
    },
  },

  conventions: {
    assets: /^(client\/assets)/
  },

  paths: {
    watched: [
      'client'
    ],
  },

  plugins: {
    babel: {
      // Do not use ES6 compiler in vendor code
      ignore: [/vendor/]
    }
  },

  // Seems to be broken right now...
  // npm: {
  //   enabled: true
  // }
};
