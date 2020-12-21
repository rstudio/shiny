/* global Selectize */
Selectize.define("selectize-plugin-a11y", function (options) {
  var self = this;
  var KEY_RETURN = 13;

  if (typeof self.accessibility === "undefined") {
    self.accessibility = {};
  }

  self.accessibility.helpers = {
    randomId: function (len) {
      var str = "",
        strLength = len || 10,
        base = "abcdefghijklmnopqrstuvwxyz0123456789",
        baseLength = base.length;

      for (var i = 0; i < strLength; i++) {
        str += base[Math.floor(baseLength * Math.random())];
      }

      return str;
    }
  };

  self.accessibility.liveRegion = {
    $region: "",
    speak: function (msg) {
      var $container = $("<div></div>");
      $container.text(msg);
      this.$region.html($container);
    },
    domListener: function () {
      var observer = new MutationObserver(function (mutations) {
        mutations.forEach(function (mutation) {
          var $target = $(mutation.target);
          if ($target.hasClass("items")) {
            if ($target.hasClass("dropdown-active")) {
              // open
              self.$control_input.attr("aria-expanded", "true");
              // Assign a unique ID for each item. This is necessary for
              // screen readers. In the selectize-plugin-a11y example, the
              // random IDs are assigned when .selectize() is called, but we're
              // doing it here to limit the scope of changes.
              var kids = self.$dropdown_content[0].children;
              for (i = 0; i < kids.length; i++) {
                var attrs = kids[i].attributes;
                if (!attrs.role) {
                  kids[i].setAttribute("role", "option");
                }
                if (!attrs.id) {
                  kids[i].setAttribute("id", self.accessibility.helpers.randomId());
                }
              }
            } else {
              // close
              self.$control_input.attr("aria-expanded", "false");
              self.$control_input.removeAttr("aria-activedescendant");
            }
          } else {
            // option change
            if ($target.hasClass("active")) {
              if (!!$target.attr("data-value")) { // eslint-disable-line no-extra-boolean-cast
                self.$control_input.attr(
                  "aria-activedescendant",
                  $target.attr("id")
                );
                self.accessibility.liveRegion.speak($target.text(), 500);
              }
            }
          }
        });
      });
      observer.observe(self.$dropdown[0], {
        attributes: true,
        attributeFilter: ["class"],
        subtree: true,
        attributeOldValue: true
      });

      observer.observe(self.$control[0], {
        attributes: true,
        attributeFilter: ["class"]
      });

      observer.observe(self.$control_input[0], {
        attributes: true,
        attributeFilter: ["value"]
      });
    },
    setAttributes: function () {
      this.$region.attr({
        "aria-live": "assertive",
        role: "log",
        "aria-relevant": "additions",
        "aria-atomic": "true"
      });
    },
    setStyles: function () {
      this.$region.css({
        position: "absolute",
        width: "1px",
        height: "1px",
        "margin-top": "-1px",
        clip: "rect(1px, 1px, 1px, 1px)",
        overflow: "hidden"
      });
    },
    init: function () {
      this.$region = $("<div>");
      this.setAttributes();
      this.setStyles();
      $("body").append(this.$region);
      this.domListener();
    }
  };

  this.setup = (function () {
    var original = self.setup;
    return function () {
      original.apply(this, arguments);
      var inputId = self.accessibility.helpers.randomId(),
        listboxId = self.accessibility.helpers.randomId();

      self.$control.on("keydown", function (e) {
        if (e.keyCode === KEY_RETURN) {

          if (self.settings.openOnFocus) {
            // This is the common case, where openOnFocus is true. When the user
            // presses Enter to select an item, the default behavior is to close
            // the dropdown and have the input lose focus. However, losing focus
            // is poor behavior for accessibility.
            //
            // If `.focus()` is called after a selection is made, the default
            // (when openOnFocus is true) is to show the dropdown, but this is
            // annoying for sighted users, because they would expect the
            // dropdown to not be visible after making a selection, and they
            // then need to press Esc to make it go away.
            //
            // This workaround sets openOnFocus to false, then calls `.focus()`,
            // then, after a delay, sets openOnFocus back to true. It seems that
            // the `setTimeout()` must be called after `.focus()` for it to work
            // reliably, even with a delay of up to 10. If it is called before
            // `.focus()`, then after pressing Enter, the dropdown still
            // appears. I think this is probably because `.focus()` in turn
            // calls `setTimeout(, 0)` to do things, calling the functions in
            // this order is necessary to ensure that the callbacks are invoked
            // in the same order.
            self.settings.openOnFocus = false;
            self.focus();
            setTimeout(function() { self.settings.openOnFocus = true; }, 0);
          } else {
            self.focus();
          }
        }
      });

      self.$control_input.attr({
        role: "combobox",
        "aria-expanded": "false",
        haspopup: "listbox",
        "aria-owns": listboxId,
        "aria-label": self.$wrapper
          .closest("[data-accessibility-selectize-label]")
          .attr("data-accessibility-selectize-label")
      });

      self.$dropdown_content.attr({
        role: "listbox",
        id: listboxId
      });
      self.accessibility.liveRegion.init();
    };
  })();

  this.destroy = (function () {
    var original = self.destroy;
    return function () {
      self.accessibility.liveRegion.$region.remove();
      return original.apply(this, arguments);
    };
  })();
});
