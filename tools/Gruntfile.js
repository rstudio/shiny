module.exports = function(grunt) {

  var instdir = '../inst/';
  var js_srcdir = '../srcjs/';
  var bundled_dir = '../bundled/';

  var sources = [
    'utils.js',
    'browser.js',
    'input_rate.js',
    'shinyapp.js',
    'notifications.js',
    'modal.js',
    'file_processor.js',
    'binding_registry.js',
    'output_binding.js',
    'output_binding_text.js',
    'output_binding_image.js',
    'output_binding_html.js',
    'output_binding_downloadlink.js',
    'output_binding_datatable.js',
    'output_binding_adapter.js',
    'input_binding.js',
    'input_binding_text.js',
    'input_binding_textarea.js',
    'input_binding_password.js',
    'input_binding_number.js',
    'input_binding_checkbox.js',
    'input_binding_slider.js',
    'input_binding_date.js',
    'input_binding_daterange.js',
    'input_binding_select.js',
    'input_binding_radio.js',
    'input_binding_checkboxgroup.js',
    'input_binding_actionbutton.js',
    'input_binding_tabinput.js',
    'input_binding_fileinput.js',
    'init_shiny.js',
    'reactlog.js'
  ].map(src => js_srcdir + src);

  var preamble = [js_srcdir + '_start.js'];
  var epilogue = [js_srcdir + '_end.js'];

  var lodash = [
    // Disables ESLint
    bundled_dir + 'lodash_preamble.js',
    bundled_dir + 'lodash.custom.js',
    // Re-enables ESLint and ensures Lodash is loaded only within the Shiny
    // closure.
    bundled_dir + 'lodash_epilogue.js'
  ];

  gruntConfig = {
    pkg: pkgInfo(),

    clean: {
      options: { force: true },
      src: [
        instdir + "www/shared/shiny.js",
        instdir + "www/shared/shiny.js.map",
        instdir + "www/shared/shiny.min.js",
        instdir + "www/shared/shiny.min.js.map",
        "./temp_concat/shiny.js",
        "./temp_concat/shiny.js.map",
        instdir + 'www/shared/datepicker/js/bootstrap-datepicker.min.js',
        instdir + 'www/shared/ionrangeslider/js/ion.rangeSlider.min.js'
      ]
    },

    concat: {
      options: {
        process: function(src, filepath) {
          return '//---------------------------------------------------------------------\n' +
            '// Source file: ' + filepath + '\n\n' + src;
        },
        sourceMap: true
      },
      shiny: {
        src: preamble.concat(lodash, sources, epilogue),
        // The temp_concat/ directory would have gone under /srcjs/, but the
        // Babel Grunt plugin has trouble finding presets if it operates on a
        // file that's not under the current directory. So we'll put it under
        // ./
        dest: './temp_concat/shiny.js',
        nonull: true
      }
    },

    babel: {
      options: {
        sourceMap: true,
        compact: false,
        presets: ['es2015']
      },
      shiny: {
        src: './temp_concat/shiny.js',
        dest: instdir + '/www/shared/shiny.js'
      }
    },

    eslint: {
      options: {
        parser: 'babel-eslint',
        format: require('eslint-stylish-mapped'),
        extends: 'eslint:recommended',
        rules: {
          "consistent-return": 1,
          "dot-location": [1, "property"],
          "eqeqeq": 1,
          // "no-shadow": 1,
          "no-undef": 1,
          "no-unused-vars": [1, {"args": "none"}],
          "guard-for-in": 1,
          // "no-use-before-define": [1, {"functions": false}],
          "semi": [1, "always"]
        },
        envs: [
          "es6",
          "browser",
          "jquery"
        ],
        globals: ["strftime"]
      },
      shiny: ['./temp_concat/shiny.js']
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

    watch: {
      shiny: {
        files: ['<%= concat.shiny.src %>', '../DESCRIPTION'],
        tasks: [
          'newer:concat',
          'newer:eslint',
          'configureBabel',
          'newer:babel',
          'newer:uglify'
        ]
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
  };

  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-babel');
  grunt.loadNpmTasks('grunt-eslint');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-newer');

  // Need this here so that babel reads in the source map file after it's
  // generated. Without this task, it would read in the source map when Grunt
  // runs, which is wrong, if the source map doesn't exist, or is change later.
  grunt.task.registerTask("configureBabel", "configures babel options", function() {
    gruntConfig.babel.options.inputSourceMap = grunt.file.readJSON('./temp_concat/shiny.js.map');
  });

  grunt.initConfig(gruntConfig);

  grunt.registerTask('default', [
    'newer:concat',
    'newer:eslint',
    'configureBabel',
    'newer:babel',
    'newer:uglify'
  ]);


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
