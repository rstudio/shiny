/*
  *  jQuery Hashchange - v1.0.0
*  A plugin which allows to bind callbacks to custom window.location.hash (uri fragment id) values.
*  https://github.com/apopelo/jquery-hashchange
*
  *  Made by Andrey Popelo
*  Under MIT License
*/
  ;(function($) {
    var methods = {
      init: function(options) {
        var settings = $.extend({
          "hash"     : "",
          "onSet"    : function(){},
          "onRemove" : function(){}
        }, options);

        if (!settings.hash) {
          return this;
        }

        // bind to hashchange at first time and init global variables
        if (!$.hashchange) {
          $.hashchange = {};
          $.hashchange.onSet = {};
          $.hashchange.onRemove = {};
          $.hashchange.prevHash = "";

          $.hashchange.listener = function() {
            // if hash didn't change - do nothing
            if (window.location.hash === $.hashchange.prevHash) {
            return;
            }

            var onRemove = $.hashchange.onRemove[$.hashchange.prevHash],
            onSet = $.hashchange.onSet[window.location.hash];

            if (onRemove) {
            onRemove();
            }

            if (onSet) {
            onSet();
            }

            $.hashchange.prevHash = window.location.hash;
          };

            this.bind("hashchange", $.hashchange.listener);
      }

            $.hashchange.onSet[settings.hash] = settings.onSet;
            $.hashchange.onRemove[settings.hash] = settings.onRemove;

            // fire hashchange if current hash equals given
            // and it is not already active
            if (window.location.hash === settings.hash &&
            window.location.hash !== $.hashchange.prevHash) {
            $.hashchange.listener();
            }

            return this;
    }
    };

            $.fn.hashchange = function(options) {
            // options array passed
            if (Object.prototype.toString.call(options) === "[object Array]") {
            for (var i = options.length - 1; i >= 0; i--) {
            methods.init.apply(this, [options[i]]);
            }
            return this;
            }
            // single option passed
            return methods.init.apply(this, arguments);
            };
  })(jQuery);
