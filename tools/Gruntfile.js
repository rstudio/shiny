module.exports = function(grunt) {

  var srcdir = '../inst/';

  grunt.initConfig({
    pkg: pkgInfo(),
    uglify: {
      shiny: {
        options: {
          banner: '/*! <%= pkg.name %> <%= pkg.version %> | ' +
                  '(c) 2012-<%= grunt.template.today("yyyy") %> RStudio, Inc. | ' +
                  'License: <%= pkg.license %> */\n',
          sourceMap: true
        },
        src: srcdir + 'www/shared/<%= pkg.name %>.js',
        dest: srcdir + 'www/shared/<%= pkg.name %>.min.js'
      },
      datepicker: {
        src: [
          srcdir + 'www/shared/datepicker/js/bootstrap-datepicker.js',
          srcdir + 'www/shared/datepicker/js/locales/bootstrap-datepicker.*.js'
        ],
        dest: srcdir + 'www/shared/datepicker/js/bootstrap-datepicker.min.js'
      }
    },

    jshint: {
      options: {
        force: true  // Don't abort if there are JSHint warnings
      },
      shiny: {
        src: srcdir + 'www/shared/shiny.js'
      }
    },

    watch: {
      shiny: {
        files: ['<%= uglify.shiny.src %>', '../DESCRIPTION'],
        tasks: ['newer:jshint:shiny', 'newer:uglify:shiny']
      },
      datepicker: {
        files: '<%= uglify.datepicker.src %>',
        tasks: ['newer:uglify:datepicker']
      }
    },

    newer: {
      options: {
        override: function(detail, include) {
          // If DESCRIPTION is updated, we'll also need to re-minify shiny.js
          // because the min.js file embeds the version number.
          if (detail.task === 'uglify' && detail.target === 'shiny') {
            include(isNewer('../DESCRIPTION', detail.time));
          } else {
            include(false);
          }

        }
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-newer');


  grunt.registerTask('default', ['newer:uglify', 'newer:jshint']);


  // ---------------------------------------------------------------------------
  // Utility functions
  // ---------------------------------------------------------------------------

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

  // Return true if file's mtime is newer than mtime; false otherwise.
  function isNewer(file, mtime) {
    return require('fs').statSync(file).mtime > mtime;
  }
};
