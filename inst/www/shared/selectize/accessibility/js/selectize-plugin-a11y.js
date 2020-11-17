/* global Selectize */
function selectize_accessibility_plugin(options) {
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
          $(this).click();
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
      var destroyed = original.apply(this, arguments);
      // Redefine plugin after destroying the instance so that selectizeInput()'s
      // receiveMessage() can call destroy and update the instance without
      // needing to reload this plugin
      Selectize.define("selectize-plugin-a11y", selectize_accessibility_plugin);
      return destroyed;
    };
  })();
}

Selectize.define("selectize-plugin-a11y", selectize_accessibility_plugin);
