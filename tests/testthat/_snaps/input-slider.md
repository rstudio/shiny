# sliderInput gives informative errors for bad inputs

    Code
      sliderInput("x", "x")
    Error <simpleError>
      argument "min" is missing, with no default
    Code
      sliderInput("x", "x", min = NULL, max = 3, value = 2)
    Error <rlang_error>
      sliderInput(min) must be a single number, Date, or POSIXct
    Code
      sliderInput("x", "x", min = 1, max = NULL, value = 2)
    Error <rlang_error>
      sliderInput(value) must be a single number, Date, or POSIXct
    Code
      sliderInput("x", "x", min = 1, max = 3, value = NULL)
    Error <rlang_error>
      sliderInput(value) must be a single or pair of numbers, Dates, or POSIXcts
    Code
      sliderInput("x", "x", min = Sys.Date(), max = Sys.Date(), value = 1)
    Error <rlang_error>
      Type mismatch for `min`, `max`, and `value`.
      i All values must have same type: either numeric, Date, or POSIXt.
    Code
      sliderInput("x", "x", min = 1, max = 3, value = 0)
    Error <rlang_error>
      `value` does not lie within [min, max]

