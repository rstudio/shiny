// https://github.com/kentcdodds/dom-testing-library-with-anything/blob/main/jquery.test.js

import "@testing-library/jest-dom/extend-expect";
import $ from "jquery";
import { getQueriesForElement } from "@testing-library/dom";
import userEvent from "@testing-library/user-event";

$.fn.internalTest = function countify(this: JQuery<HTMLElement>) {
  this.html(`
    <div>
      <button>0</button>
    </div>
  `);
  const $button = this.find("button");

  // JQuery<HTMLButtonElement>

  let count = 0;

  $button.click(() => {
    count++;
    $button.text(count);
  });
};

// tests:
test("counter increments", () => {
  const div = document.createElement("div");

  $(div).internalTest();

  const { getByText } = getQueriesForElement(div);
  const counter = getByText("0");

  userEvent.click(counter);
  expect(counter).toHaveTextContent("1");

  userEvent.click(counter);
  expect(counter).toHaveTextContent("2");
});
