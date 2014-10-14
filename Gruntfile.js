module.exports = function(grunt) {

  // Load all grunt taks matching grunt-*
  require('load-grunt-tasks')(grunt);
  
  // Project config
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    sass: {
      dist: {
        files: {
          'output/main.css': 'scss/main.scss'
        }
      }
    }
  });

  grunt.registerTask('default', 'Compile SCSS', ['sass']);
};
