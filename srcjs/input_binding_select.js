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
    var selectize = this._selectize(el);
    if (typeof(selectize) !== 'undefined') {
      selectize.setValue(value);
    } else $(el).val(value);
  },
  getState: function(el) {
    // Store options in an array of objects, each with with value and label
    var options = new Array(el.length);
    for (var i = 0; i < el.length; i++) {
      options[i] = { value:    el[i].value,
                     label:    el[i].label };
    }

    return {
      label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
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
      selectize.clearOptionGroups();
      var thiz = this, loaded = false;
      selectize.settings.load = function(query, callback) {
        var settings = selectize.settings;
        var thiz2 = this;
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
            $.each(res, function(index, elem) {
              thiz2.addOptionGroup(elem['group'], { group: elem['group'] });
            });
            thiz2.refreshOptions();
            callback(res);
            if (!loaded && data.hasOwnProperty('value')) {
              thiz.setValue(el, data.value);
            } else {
              thiz2.addItem(res[0].value);
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

    if (data.hasOwnProperty('label'))
      $(el).parent().parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

    $(el).trigger('change');
  },
  subscribe: function(el, callback) {
    $(el).on('change.selectInputBinding', function(event) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.selectInputBinding');
  },
  initialize: function(el) {
    this._selectize(el);
  },
  _selectize: function(el, update) {
    if (!$.fn.selectize) return undefined;
    var $el = $(el);
    var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
    if (config.length === 0) return undefined;
    var options = $.extend({
      labelField: 'label',
      valueField: 'value',
      searchField: ['label'],
      optgroupField: 'group',
			optgroupLabelField: 'group',
			optgroupValueField: 'group'
    }, JSON.parse(config.html()));
    // selectize created from selectInput()
    if (typeof(config.data('nonempty')) !== 'undefined') {
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
