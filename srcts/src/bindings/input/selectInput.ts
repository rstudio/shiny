import $ from "jquery";
import { $escape, hasDefinedProperty, updateLabel } from "../../utils";
import { indirectEval } from "../../utils/eval";
import { InputBinding } from "./inputBinding";

type SelectHTMLElement = HTMLSelectElement & { nonempty: boolean };

type SelectInputReceiveMessageData = {
  label: string;
  options?: string;
  config?: string;
  url?: string;
  value?: string;
};

type SelectizeOptions = Selectize.IOptions<string, unknown>;
type SelectizeInfo = Selectize.IApi<string, unknown> & {
  settings: SelectizeOptions;
};

function getLabelNode(el: SelectHTMLElement): JQuery<HTMLElement> {
  let escapedId = $escape(el.id);

  if (isSelectize(el)) {
    escapedId += "-selectized";
  }
  return $(el)
    .parent()
    .parent()
    .find('label[for="' + escapedId + '"]');
}

function getConfigScript(el: SelectHTMLElement): JQuery<HTMLScriptElement> {
  return $(el)
    .parent()
    .find('script[data-for="' + $escape(el.id) + '"]');
}

// Return true if it's a selectize input, false if it's a regular select input.

function isSelectize(el: SelectHTMLElement): boolean {
  const config = getConfigScript(el);

  return config.length > 0;
}

class SelectInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    // Inputs also have .shiny-input-select class
    return $(scope).find("select");
  }
  getType(el: HTMLElement): string | null {
    const $el = $(el);

    if (!$el.hasClass("symbol")) {
      // default character type
      return null;
    }
    if ($el.attr("multiple") === "multiple") {
      return "shiny.symbolList";
    } else {
      return "shiny.symbol";
    }
  }
  getId(el: SelectHTMLElement): string {
    return InputBinding.prototype.getId.call(this, el) || el.name;
  }
  getValue(el: SelectHTMLElement): any {
    if (!isSelectize(el)) {
      return $(el).val();
    } else {
      const selectize = this._selectize(el);

      return selectize?.getValue();
    }
  }
  setValue(el: SelectHTMLElement, value: string): void {
    if (!isSelectize(el)) {
      $(el).val(value);
    } else {
      const selectize = this._selectize(el);

      selectize?.setValue(value);
    }
  }
  getState(el: SelectHTMLElement): {
    label: JQuery<HTMLElement>;
    value: ReturnType<SelectInputBinding["getValue"]>;
    options: Array<{ value: string; label: string }>;
  } {
    // Store options in an array of objects, each with with value and label
    const options: Array<{ value: string; label: string }> = new Array(
      el.length,
    );

    for (let i = 0; i < el.length; i++) {
      options[i] = {
        // TODO-barret; Is this a safe assumption?; Are there no Option Groups?
        value: (el[i] as HTMLOptionElement).value,
        label: el[i].label,
      };
    }

    return {
      label: getLabelNode(el),
      value: this.getValue(el),
      options: options,
    };
  }
  async receiveMessage(
    el: SelectHTMLElement,
    data: SelectInputReceiveMessageData,
  ): Promise<void> {
    const $el = $(el);

    // This will replace all the options
    if (hasDefinedProperty(data, "options")) {
      const selectize = this._selectize(el);

      // Must destroy selectize before appending new options, otherwise
      // selectize will restore the original select
      selectize?.destroy();
      // Clear existing options and add each new one
      $el.empty().append(data.options!);
      this._selectize(el);
    }

    if (hasDefinedProperty(data, "config")) {
      const oldConfig = getConfigScript(el);

      // Before replacing the config, remember the remove-button (for py-shiny)
      const oldRemoveButton = oldConfig[0].getAttribute("data-remove-button");

      // Replace the old config with the new one
      oldConfig.replaceWith(data.config!);

      // If remove-button was present in the old but not the new config,
      // keep it since it should be sticky across updates
      const newConfig = getConfigScript(el);
      const newRemoveButton = newConfig[0].getAttribute("data-remove-button");
      if (oldRemoveButton !== null && newRemoveButton === null) {
        newConfig[0].setAttribute("data-remove-button", oldRemoveButton);
      }

      // re-initialize selectize (with the new config)
      this._selectize(el, true);
    }

    // use server-side processing for selectize
    if (hasDefinedProperty(data, "url")) {
      type CallbackFn = Parameters<
        NonNullable<SelectizeInfo["settings"]["load"]>
      >[1];
      const selectize = this._selectize(el) as ReturnType<
        SelectInputBinding["_selectize"]
      > & {
        settings: {
          load: (query: string, callback: CallbackFn) => any;
        };
      };

      // Calling selectize.clear() first works around https://github.com/selectize/selectize.js/issues/2146
      // As of selectize.js >= v0.13.1, .clearOptions() clears the selection,
      // but does NOT remove the previously-selected options. So unless we call
      // .clear() first, the current selection(s) will remain as (deselected)
      // options. See #3966 #4142
      selectize.clear();
      selectize.clearOptions();
      let loaded = false;

      selectize.settings.load = function (query: string, callback: CallbackFn) {
        const settings = selectize.settings;

        $.ajax({
          url: data.url,
          data: {
            query: query,
            field: JSON.stringify([settings.searchField]),
            value: settings.valueField,
            conju: settings.searchConjunction,
            maxop: settings.maxOptions,
          },
          type: "GET",
          error: function () {
            callback();
          },
          success: function (res) {
            // res = [{label: '1', value: '1', group: '1'}, ...]
            // success is called after options are added, but
            // groups need to be added manually below
            $.each(res, function (index, elem) {
              // Call selectize.addOptionGroup once for each optgroup; the
              // first argument is the group ID, the second is an object with
              // the group's label and value. We use the current settings of
              // the selectize object to decide the fieldnames of that obj.
              const optgroupId = elem[settings.optgroupField || "optgroup"];
              const optgroup: { [key: string]: string } = {};

              optgroup[settings.optgroupLabelField || "label"] = optgroupId;
              optgroup[settings.optgroupValueField || "value"] = optgroupId;
              selectize.addOptionGroup(optgroupId, optgroup);
            });
            callback(res);
            if (!loaded) {
              if (hasDefinedProperty(data, "value")) {
                selectize.setValue(data.value as any);
              } else if (settings.maxItems === 1) {
                // first item selected by default only for single-select
                selectize.setValue(res[0].value);
              }
            }
            loaded = true;
          },
        });
      };
      // perform an empty search after changing the `load` function
      selectize.load(function (callback) {
        selectize.settings.load.apply(selectize, ["", callback]);
      });
    } else if (hasDefinedProperty(data, "value")) {
      // @ts-expect-error; data.value is currently a never type
      this.setValue(el, data.value);
    }

    await updateLabel(data.label, getLabelNode(el));

    $(el).trigger("change");
  }
  subscribe(el: SelectHTMLElement, callback: (x: boolean) => void): void {
    $(el).on(
      "change.selectInputBinding",
      // event: Event
      () => {
        // https://github.com/rstudio/shiny/issues/2162
        // Prevent spurious events that are gonna be squelched in
        // a second anyway by the onItemRemove down below
        if (el.nonempty && this.getValue(el) === "") {
          return;
        }
        callback(false);
      },
    );
  }
  unsubscribe(el: HTMLElement): void {
    $(el).off(".selectInputBinding");
  }
  initialize(el: SelectHTMLElement): void {
    this._selectize(el);
  }
  protected _selectize(
    el: SelectHTMLElement,
    update = false,
  ): SelectizeInfo | undefined {
    // Apps like 008-html do not have the selectize js library
    // Safe-guard against missing the selectize js library
    if (!$.fn.selectize) return undefined;
    const $el = $(el);
    const config = getConfigScript(el);

    if (config.length === 0) return undefined;

    this._addRemoveButtonPlugins(el, config[0]);

    let options: SelectizeOptions & {
      labelField: "label";
      valueField: "value";
      searchField: ["label"];
      onItemRemove?: (value: string) => void;
      onDropdownClose?: () => void;
    } = $.extend(
      {
        labelField: "label",
        valueField: "value",
        searchField: ["label"],
      },
      JSON.parse(config.html()),
    );

    // selectize created from selectInput()
    if (typeof config.data("nonempty") !== "undefined") {
      el.nonempty = true;
      options = $.extend(options, {
        onItemRemove: function (this: SelectizeInfo, value: string) {
          if (this.getValue() === "")
            $("select#" + $escape(el.id))
              .empty()
              .append(
                $("<option/>", {
                  value: value,
                  selected: true,
                }),
              )
              .trigger("change");
        },
        onDropdownClose:
          // $dropdown: any
          function (this: SelectizeInfo) {
            if (this.getValue() === "") {
              this.setValue($("select#" + $escape(el.id)).val() as string);
            }
          },
      });
    } else {
      el.nonempty = false;
    }
    // options that should be eval()ed
    if (config.data("eval") instanceof Array)
      $.each(config.data("eval"), function (i, x: string) {
        /*jshint evil: true*/
        // @ts-expect-error; Need to type `options` keys to know exactly which values are accessed.
        options[x] = indirectEval("(" + options[x] + ")");
      });
    let control = $el.selectize(options)[0].selectize as SelectizeInfo;
    // .selectize() does not really update settings; must destroy and rebuild

    if (update) {
      const settings = $.extend(control.settings, options);

      control.destroy();
      control = $el.selectize(settings)[0].selectize as SelectizeInfo;
    }

    return control;
  }

  // py-shiny may include a data-remove-button attribute, requesting
  // "default" plugins (i.e., plugins not specified by the user).
  // If present, update the config (i.e., the <script> tag's JSON object)
  // to include them.
  private _addRemoveButtonPlugins(
    el: SelectHTMLElement,
    config: HTMLScriptElement,
  ): void {
    if (!config.hasAttribute("data-remove-button")) return;

    const removeButton = config.getAttribute("data-remove-button");

    const plugins = [];
    if (removeButton == "both") {
      plugins.push("remove_button", "clear_button");
    } else if (removeButton == "true") {
      plugins.push(el.multiple ? "remove_button" : "clear_button");
    }

    const configJSON: SelectizeOptions = JSON.parse(config.innerHTML);
    const pluginsJSON = configJSON.plugins || [];

    plugins.forEach((plugin) => {
      if (!pluginsJSON.includes(plugin)) {
        pluginsJSON.push(plugin);
      }
    });

    configJSON.plugins = pluginsJSON;
    config.innerHTML = JSON.stringify(configJSON);
  }
}

export { SelectInputBinding };
export type { SelectInputReceiveMessageData };
