module.exports = function(grunt) {

  // Load all grunt taks matching grunt-*
  require('load-grunt-tasks')(grunt);

  // Project config
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    sass: {
      dist: {
        files: {
          'output/Site.css': 'scss/main.scss'
        }
      }
    },

    // Lint JS for mistakes
    jshint: {
      files: 'js/**/*.js',
      options: {
        force: true
      }
    },

    // Concat JS into one file
    concat: {
      dist: {
        src: '<%- jshint.files %>',
        dest: 'output/main.js'
      }
    },

    // // Deploy to FTP test group
    // 'ftp-deploy': {
    //   build: {
    //     auth: {
    //       host: 'FIXME',
    //       port: 'FIXME',
    //       authPath: 'ftppass.json'
    //     },
    //     src: 'output',
    //     dest: 'FIXME',
    //     exclusions: ['*.DS_Store', '*.keep']
    //   }
    // },

    // Watch for changes and rerun tasks
    watch: {
      sass: {
        files: 'scss/**/*',
        tasks: 'sass'
      },
      js: {
        files: '<%= jshint.files %>',
        tasks: ['jshint', 'concat']
      }
    }
  });

  grunt.registerTask(
      'default',
      'Compile files and watch',
      ['sass', 'jshint', 'concat', 'watch']);
};
