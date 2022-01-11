/* global module */
module.exports = function(config) {
  'use strict';
  
  config.set({

    basePath:   '',
    port:       9876,
    colors:     true,
    autoWatch:  true,
    singleRun:  false,
    reporters:  ['progress'],
    browsers:   ['PhantomJS'],
    frameworks: ['mocha', 'chai'],

    files: [
      'public/main.js',
      'test/**/*_test.js',
    ],
    exclude: [],

    preprocessors: {
      'test/**/*_test.js': ['babel'],
      '**.*/js': ['jshint'],
    },

    // possible values:
    //  config.LOG_DISABLE
    //  config.LOG_ERROR
    //  config.LOG_WARN
    //  config.LOG_INFO
    //  config.LOG_DEBUG
    logLevel: config.LOG_INFO,
  });
};
