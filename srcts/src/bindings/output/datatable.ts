import $ from "jquery";

import { shinyUnbindAll } from "../../shiny/initedMethods";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
import { debounce } from "../../time";
import { escapeHTML } from "../../utils";
import { indirectEval } from "../../utils/eval";
import { OutputBinding } from "./outputBinding";

class DatatableOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-datatable-output");
  }
  onValueError(el: HTMLElement, err: ErrorsMessageValue): void {
    shinyUnbindAll(el);
    this.renderError(el, err);
  }
  renderValue(
    el: HTMLElement,
    data: {
      colnames?: string[];
      options?: {
        searching?: boolean;
        search?: { caseInsensitive?: boolean };
        // To be sent to data table;
        // Will copy in R value to this location
        escape?: string;
      } | null;
      escape?: string; // Incoming from R
      action?: string;
      evalOptions?: string[];
      callback?: string;
      searchDelay?: number;
    } | null
  ): void {
    const $el = $(el).empty();

    if (!data || !data.colnames) return;

    const colnames = $.makeArray(data.colnames);
    let header = $.map(colnames, function (x) {
      return "<th>" + x + "</th>";
    }).join("");

    header = "<thead><tr>" + header + "</tr></thead>";
    let footer = "";

    if (data.options?.searching ?? true) {
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
    if (data.evalOptions) {
      $.each(data.evalOptions, function (i, x) {
        /*jshint evil: true */
        // @ts-expect-error; If `evalOptions` is defined, `data.options` should be defined
        data.options[x] = indirectEval("(" + data.options[x] + ")");
      });
    }

    // caseInsensitive searching? default true
    const searchCI = data.options?.search?.caseInsensitive !== false;
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
              data: function (d: NonNullable<typeof data.options>) {
                d.search || (d.search = {});
                d.search.caseInsensitive = searchCI;
                // Copy from the R value (`data.escape`) to the escape option
                // (`d.escape`) similar to `data.options.escape`;
                // Note: this logic may be wrong, but the method is strongly
                // deprecated in favor of DT package. So users should not
                // naturally run this line of code
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
        debounce(data.searchDelay, function (this: HTMLInputElement) {
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
        debounce(data.searchDelay, function (this: HTMLInputElement) {
          oTable.column(searchInputs.index(this)).search(this.value).draw();
        })
      );
    }
    // FIXME: ugly scrollbars in tab panels b/c Bootstrap uses 'visible: auto'
    $el.parents(".tab-content").css("overflow", "visible");
  }
}

export { DatatableOutputBinding };
