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
          'output/CSS/Site.css': 'scss/main.scss'
        }
      },
      prod: {
        files: {
          'output/CSS/Site.css': 'scss/main.scss'
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
        dest: 'output/JS/main.js'
      }
    },

    // Minify JS
    uglify: {
      prod: {
        files: {
          'output/JS/main.min.js': 'output/JS/main.js'
        }
      }
    },

    // Deploy to FTP test group
    'ftp-deploy': {
      dev: {
        auth: {
          host: 'russellinvestments.roi360.co.uk',
          port: 21,
          authPath: './ftppass.json',
          authKey: 'site'
        },
        src: 'output',
        dest: 'Themes/Russell-Skinning/',
        exclusions: ['.DS_Store', '.keep']
      }
    },

    // Watch for changes and rerun tasks
    watch: {
      sass: {
        files: 'scss/**/*',
        tasks: ['sass:dev', 'ftp-deploy:dev']
      },
      js: {
        files: jsFiles,
        tasks: ['jshint:dev', 'concat', 'ftp-deploy:dev']
      }
    }
  });

  grunt.registerTask(
    'default',
    'Compile files and watch',
    ['sass:dev', 'jshint:dev', 'concat', 'ftp-deploy:dev', 'watch']);

  grunt.registerTask(
    'deploy',
    'Compile files ready for deployment',
    ['sass:prod', 'jshint:prod', 'concat', 'uglify']);
};
