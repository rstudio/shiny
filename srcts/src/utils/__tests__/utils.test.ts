import { asArray, mergeSort } from "../";

test("integer is converted to integer array", () => {
  expect(asArray(4)).toStrictEqual([4]);
});

test("mergeSort merges two arrays", () => {
  function shuffleArray<T>(arr: Array<T>): Array<T> {
    if (arr.length < 2) return arr;

    const copy = [...arr];

    copy.sort(() => Math.random() - 0.5);
    for (let i = 0; i < copy.length; i++) {
      // if at least one entry is different, return it
      if (arr[i] !== copy[i]) {
        return copy;
      }
    }
    // Try shuffling again (rare)
    return shuffleArray(arr);
  }

  const original = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5];
  const input = shuffleArray(original);

  const output = mergeSort(input, function (x, y) {
    return x - y;
  });

  expect(original).not.toEqual(input);
  expect(original).toEqual(output);
});
