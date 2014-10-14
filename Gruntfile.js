module.exports = function(grunt) {
  var jsFiles = 'js/**/*.js';

  // Load all grunt taks matching grunt-*
  require('load-grunt-tasks')(grunt);

  // Project config
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    sass: {
      dev: {
        files: {
          'output/Site.css': 'scss/main.scss'
        }
      },
      prod: {
        files: {
          'output/Site.css': 'scss/main.scss'
        },
        options: {
          outputStyle: 'compressed'
        }
      }
    },

    // Lint JS for mistakes
    jshint: {
      dev: {
        src: jsFiles,
        options: {
          force: true
        }
      },
      prod: {
        src: jsFiles,
        options: {
          force: false
        }
      }
    },

    // Concat JS into one file
    concat: {
      dist: {
        src: jsFiles,
        dest: 'output/main.js'
      }
    },

    // Minify JS
    uglify: {
      prod: {
        files: {
          'output/main.min.js': 'output/main.js'
        }
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
        files: jsFiles,
        tasks: ['jshint:dev', 'concat']
      }
    }
  });

  grunt.registerTask(
    'default',
    'Compile files and watch',
    ['sass:dev', 'jshint:dev', 'concat', 'watch']);

  grunt.registerTask(
    'build',
    'Compile files ready for deployment',
    ['sass:prod', 'jshint:prod', 'concat', 'uglify']);
};
