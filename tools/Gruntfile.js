module.exports = function(grunt) {

  var srcdir = '../inst/';

  grunt.initConfig({
    pkg: pkgInfo(),
    uglify: {
      options: {
        banner: '/*! <%= pkg.name %> <%= pkg.version %> | ' +
                '(c) 2012-<%= grunt.template.today("yyyy") %> RStudio, Inc. | ' +
                'License: <%= pkg.license %> */\n'
      },
      build: {
        src: srcdir + 'www/shared/<%= pkg.name %>.js',
        dest: srcdir + 'www/shared/<%= pkg.name %>.min.js'
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-uglify');

  grunt.registerTask('default', ['uglify']);



  // Return an object which merges information from package.json and the
  // DESCRIPTION file.
  function pkgInfo() {
    var pkg = grunt.file.readJSON('package.json');

    pkg.name    = descKeyValue('Package');
    pkg.version = descKeyValue('Version');
    pkg.license = descKeyValue('License');

    return pkg;
  }

  // From the DESCRIPTION file, get the value of a key. This presently only
  // works if the value is on one line, the same line as the key.
  function descKeyValue(key) {
    var lines = require('fs').readFileSync('../DESCRIPTION', 'utf8').split('\n');

    var pattern = new RegExp('^' + key + ':');
    var txt = lines.filter(function(line) {
      return pattern.test(line);
    });

    txt = txt[0];

    pattern = new RegExp(key + ': *');
    txt = txt.replace(pattern, '');

    return txt;
  }
};

