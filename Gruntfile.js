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

    watch: {
      sass: {
        files: ['scss/**/*'],
        tasks: ['sass']
      }
    },

    'ftp-deploy': {
      build: {
        auth: {
          host: 'FIXME',
          port: 'FIXME',
          authPath: 'ftppass.json'
        },
        src: 'output',
        dest: 'TODO',
        exclusions: ['*.DS_Store', '*.keep']
      }
    }
  });

  grunt.registerTask(
      'default', 'Compile SCSS and watch', ['sass', 'watch']);
};
