var selectInputBinding = new InputBinding();
$.extend(selectInputBinding, {
  find: function(scope) {
    return $(scope).find('select');
  },
  getType: function(el) {
    var $el = $(el);
    if (!$el.hasClass("symbol")) {
      // default character type
      return null;
    }
    if ($el.attr("multiple") === "multiple") {
      return 'shiny.symbolList';
    } else {
      return 'shiny.symbol';
    }
  },
  getId: function(el) {
    return InputBinding.prototype.getId.call(this, el) || el.name;
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    if (!this._is_selectize(el)) {
      $(el).val(value);
    } else {
      let selectize = this._selectize(el);
      if (selectize) {
        selectize.setValue(value);
      }
    }
  },
  getState: function(el) {
    // Store options in an array of objects, each with with value and label
    var options = new Array(el.length);
    for (var i = 0; i < el.length; i++) {
      options[i] = { value:    el[i].value,
                     label:    el[i].label };
    }

    return {
      label:    this._getLabelNode(el),
      value:    this.getValue(el),
      options:  options
    };
  },
  receiveMessage: function(el, data) {
    var $el = $(el), selectize;

    // This will replace all the options
    if (data.hasOwnProperty('options')) {
      selectize = this._selectize(el);
      // Must destroy selectize before appending new options, otherwise
      // selectize will restore the original select
      if (selectize) selectize.destroy();
      // Clear existing options and add each new one
      $el.empty().append(data.options);
      this._selectize(el);
    }

    // re-initialize selectize
    if (data.hasOwnProperty('config')) {
      $el.parent()
         .find('script[data-for="' + $escape(el.id) + '"]')
         .replaceWith(data.config);
      this._selectize(el, true);
    }

    // use server-side processing for selectize
    if (data.hasOwnProperty('url')) {
      selectize = this._selectize(el);
      selectize.clearOptions();
      var loaded = false;
      selectize.settings.load = function(query, callback) {
        var settings = selectize.settings;
        $.ajax({
          url: data.url,
          data: {
            query: query,
            field: JSON.stringify([settings.searchField]),
            value: settings.valueField,
            conju: settings.searchConjunction,
            maxop: settings.maxOptions
          },
          type: 'GET',
          error: function() {
            callback();
          },
          success: function(res) {
            // res = [{label: '1', value: '1', group: '1'}, ...]
            // success is called after options are added, but
            // groups need to be added manually below
            $.each(res, function(index, elem) {
              // Call selectize.addOptionGroup once for each optgroup; the
              // first argument is the group ID, the second is an object with
              // the group's label and value. We use the current settings of
              // the selectize object to decide the fieldnames of that obj.
              let optgroupId = elem[settings.optgroupField || "optgroup"];
              let optgroup = {};
              optgroup[settings.optgroupLabelField || "label"] = optgroupId;
              optgroup[settings.optgroupValueField || "value"] = optgroupId;
              selectize.addOptionGroup(optgroupId, optgroup);
            });
            callback(res);
            if (!loaded) {
              if (data.hasOwnProperty('value')) {
                selectize.setValue(data.value);
              } else if (settings.maxItems === 1) {
                // first item selected by default only for single-select
                selectize.setValue(res[0].value);
              }
            }
            loaded = true;
          }
        });
      };
      // perform an empty search after changing the `load` function
      selectize.load(function(callback) {
        selectize.settings.load.apply(selectize, ['', callback]);
      });
    } else if (data.hasOwnProperty('value')) {
      this.setValue(el, data.value);
    }

    updateLabel(data.label, this._getLabelNode(el));

    $(el).trigger('change');
  },
  subscribe: function(el, callback) {
    $(el).on('change.selectInputBinding', event => {
      // https://github.com/rstudio/shiny/issues/2162
      // Prevent spurious events that are gonna be squelched in
      // a second anyway by the onItemRemove down below
      if (el.nonempty && this.getValue(el) === "") {
        return;
      }
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.selectInputBinding');
  },
  initialize: function(el) {
    this._selectize(el);
  },
  _getLabelNode: function(el) {
    let escaped_id = $escape(el.id);
    if (this._is_selectize(el)) {
      escaped_id += "-selectized";
    }
    return $(el).parent().parent().find('label[for="' + escaped_id + '"]');
  },
  // Return true if it's a selectize input, false if it's a regular select input.
  _is_selectize: function(el) {
    var config = $(el).parent().find('script[data-for="' + $escape(el.id) + '"]');
    return (config.length > 0);
  },
  _selectize: function(el, update) {
    if (!$.fn.selectize) return undefined;
    var $el = $(el);
    var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
    if (config.length === 0) return undefined;

    var options = $.extend({
      labelField: 'label',
      valueField: 'value',
      searchField: ['label']
    }, JSON.parse(config.html()));

    // selectize created from selectInput()
    if (typeof(config.data('nonempty')) !== 'undefined') {
      el.nonempty = true;
      options = $.extend(options, {
        onItemRemove: function(value) {
          if (this.getValue() === "")
            $("select#" + $escape(el.id)).empty().append($("<option/>", {
              "value": value,
              "selected": true
            })).trigger("change");
        },
        onDropdownClose: function($dropdown) {
          if (this.getValue() === "")
            this.setValue($("select#" + $escape(el.id)).val());
        }
      });
    } else {
      el.nonempty = false;
    }
    // options that should be eval()ed
    if (config.data('eval') instanceof Array)
      $.each(config.data('eval'), function(i, x) {
        /*jshint evil: true*/
        options[x] = eval('(' + options[x] + ')');
      });
    var control = $el.selectize(options)[0].selectize;
    // .selectize() does not really update settings; must destroy and rebuild
    if (update) {
      var settings = $.extend(control.settings, options);
      control.destroy();
      control = $el.selectize(settings)[0].selectize;
    }
    return control;
  }
});
inputBindings.register(selectInputBinding, 'shiny.selectInput');
