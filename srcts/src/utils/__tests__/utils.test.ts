import { asArray } from "../";

test("integer is converted to integer array", () => {
  expect(asArray(4)).toStrictEqual([4]);
});
