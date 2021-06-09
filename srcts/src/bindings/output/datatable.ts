import $ from "jquery";

import { OutputBinding } from "./OutputBinding";
import { shinyUnbindAll } from "../../shiny/initedMethods";
import { debounce } from "../../time";
import { escapeHTML } from "../../utils";
import { indirectEval } from "../../utils/eval";
import type { errorsMessageValue } from "../../shiny/shinyapp";

class DatatableOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-datatable-output");
  }
  onValueError(el: HTMLElement, err: errorsMessageValue): void {
    shinyUnbindAll(el);
    this.renderError(el, err);
  }
  renderValue(
    el: HTMLElement,
    data: null | {
      colnames?: Array<string>;
      options?: null | {
        searching?: boolean;
        search?: { caseInsensitive?: boolean };
      };
      action?: string;
      escape?: string;
      evalOptions?: Array<string>;
      callback?: string;
      searchDelay?: number;
    }
  ): void {
    const $el = $(el).empty();

    if (!data || !data.colnames) return;

    const colnames = $.makeArray(data.colnames);
    let header = $.map(colnames, function (x) {
      return "<th>" + x + "</th>";
    }).join("");

    header = "<thead><tr>" + header + "</tr></thead>";
    let footer = "";

    if (data.options === null || data.options.searching !== false) {
      footer = $.map(colnames, function (x) {
        // placeholder needs to be escaped (and HTML tags are stripped off)
        return (
          '<th><input type="text" placeholder="' +
          escapeHTML(x.replace(/(<([^>]+)>)/gi, "")) +
          '" /></th>'
        );
      }).join("");
      footer = "<tfoot>" + footer + "</tfoot>";
    }
    const content =
      '<table class="table table-striped table-hover">' +
      header +
      footer +
      "</table>";

    $el.append(content);

    // options that should be eval()ed
    if (data.evalOptions)
      $.each(data.evalOptions, function (i, x) {
        /*jshint evil: true */
        data.options[x] = indirectEval("(" + data.options[x] + ")");
      });

    // caseInsensitive searching? default true
    const searchCI =
      data.options === null ||
      typeof data.options.search === "undefined" ||
      data.options.search.caseInsensitive !== false;
    const oTable = $(el)
      .children("table")
      .DataTable(
        $.extend(
          {
            processing: true,
            serverSide: true,
            order: [],
            orderClasses: false,
            pageLength: 25,
            ajax: {
              url: data.action,
              type: "POST",
              data: function (d) {
                d.search.caseInsensitive = searchCI;
                d.escape = data.escape;
              },
            },
          },
          data.options
        )
      );
    // the table object may need post-processing

    if (typeof data.callback === "string") {
      /*jshint evil: true */
      const callback = indirectEval("(" + data.callback + ")");

      if (typeof callback === "function") callback(oTable);
    }

    // use debouncing for searching boxes
    $el
      .find("label input")
      .first()
      .unbind("keyup")
      .keyup(
        debounce(data.searchDelay, function () {
          oTable.search(this.value).draw();
        })
      );
    const searchInputs = $el.find("tfoot input");

    if (searchInputs.length > 0) {
      // this is a little weird: aoColumns/bSearchable are still in DT 1.10
      // https://github.com/DataTables/DataTables/issues/388
      $.each(oTable.settings()[0].aoColumns, function (i, x) {
        // hide the text box if not searchable
        if (!x.bSearchable) searchInputs.eq(i as number).hide();
      });
      searchInputs.keyup(
        debounce(data.searchDelay, function () {
          oTable.column(searchInputs.index(this)).search(this.value).draw();
        })
      );
    }
    // FIXME: ugly scrollbars in tab panels b/c Bootstrap uses 'visible: auto'
    $el.parents(".tab-content").css("overflow", "visible");
  }
}

export { DatatableOutputBinding };
