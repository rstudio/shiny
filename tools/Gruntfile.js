module.exports = function(grunt) {

  var instdir = '../inst/';
  var js_srcdir = '../srcjs/'

  grunt.initConfig({
    pkg: pkgInfo(),

    concat: {
      options: {
        process: function(src, filepath) {
          return '//---------------------------------------------------------------------\n' +
            '// Source file: ' + filepath + '\n\n' + src;
        },
        sourceMap: true
      },
      shiny: {
        src: [
          js_srcdir + '_start.js',
          js_srcdir + 'utils.js',
          js_srcdir + 'browser.js',
          js_srcdir + 'input_rate.js',
          js_srcdir + 'shinyapp.js',
          js_srcdir + 'file_processor.js',
          js_srcdir + 'binding_registry.js',
          js_srcdir + 'output_binding.js',
          js_srcdir + 'output_binding_text.js',
          js_srcdir + 'output_binding_image.js',
          js_srcdir + 'output_binding_html.js',
          js_srcdir + 'output_binding_downloadlink.js',
          js_srcdir + 'output_binding_datatable.js',
          js_srcdir + 'output_binding_adapter.js',
          js_srcdir + 'input_binding.js',
          js_srcdir + 'input_binding_text.js',
          js_srcdir + 'input_binding_textarea.js',
          js_srcdir + 'input_binding_number.js',
          js_srcdir + 'input_binding_checkbox.js',
          js_srcdir + 'input_binding_slider.js',
          js_srcdir + 'input_binding_date.js',
          js_srcdir + 'input_binding_daterange.js',
          js_srcdir + 'input_binding_select.js',
          js_srcdir + 'input_binding_radio.js',
          js_srcdir + 'input_binding_checkboxgroup.js',
          js_srcdir + 'input_binding_actionbutton.js',
          js_srcdir + 'input_binding_tabinput.js',
          js_srcdir + 'input_binding_fileinput.js',
          js_srcdir + 'init_shiny.js',
          js_srcdir + '_end.js'
        ],
        dest: instdir + 'www/shared/shiny.js',
        nonull: true
      },
    },

    uglify: {
      shiny: {
        options: {
          banner: '/*! <%= pkg.name %> <%= pkg.version %> | ' +
                  '(c) 2012-<%= grunt.template.today("yyyy") %> RStudio, Inc. | ' +
                  'License: <%= pkg.license %> */\n',
          sourceMap: true,
          // Base the .min.js sourcemap off of the .js sourcemap created by concat
          sourceMapIn: instdir + 'www/shared/shiny.js.map',
          sourceMapIncludeSources: true
        },
        src: instdir + 'www/shared/shiny.js',
        dest: instdir + 'www/shared/shiny.min.js'
      },
      datepicker: {
        src: [
          instdir + 'www/shared/datepicker/js/bootstrap-datepicker.js',
          instdir + 'www/shared/datepicker/js/locales/bootstrap-datepicker.*.js'
        ],
        dest: instdir + 'www/shared/datepicker/js/bootstrap-datepicker.min.js'
      },
      ionrangeslider: {
        src: instdir + 'www/shared/ionrangeslider/js/ion.rangeSlider.js',
        dest: instdir + 'www/shared/ionrangeslider/js/ion.rangeSlider.min.js'
      }
    },

    jshint: {
      options: {
        force: true  // Don't abort if there are JSHint warnings
      },
      shiny: {
        src: instdir + 'www/shared/shiny.js'
      }
    },

    watch: {
      shiny: {
        files: ['<%= concat.shiny.src %>', '../DESCRIPTION'],
        tasks: ['newer:concat', 'newer:uglify:shiny', 'newer:jshint:shiny']
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

  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-newer');


  grunt.registerTask('default', ['newer:concat', 'newer:uglify', 'newer:jshint']);


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
