// Shift an array of values so that they are within a min and max. The vals
// will be shifted so that they maintain the same spacing internally. If the
// range in vals is larger than the range of min and max, the result might not
// make sense.
function shiftToRange(
  vals: number[] | number,
  min: number,
  max: number,
): number[] {
  if (!(vals instanceof Array)) vals = [vals];

  const maxval = Math.max.apply(null, vals);
  const minval = Math.min.apply(null, vals);
  let shiftAmount = 0;

  if (maxval > max) {
    shiftAmount = max - maxval;
  } else if (minval < min) {
    shiftAmount = min - minval;
  }

  const newvals = [];

  for (let i = 0; i < vals.length; i++) {
    newvals[i] = vals[i] + shiftAmount;
  }
  return newvals;
}

export { shiftToRange };
