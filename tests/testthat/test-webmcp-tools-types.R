# The shape of a WebMCP tool's `arguments`: how they are defined, turned into
# JSON Schema, and checked on the way in (agent -> R) and out (R -> agent).
skip_if_not_installed("ellmer")
skip_if_not_installed("S7")

# Helpers --------------------------------------------------------------------

# A tool whose `fun` takes exactly the given arguments
tool_with <- function(arguments, ...) {
  fun <- function() NULL
  formals(fun) <- stats::setNames(
    rep(list(quote(expr = )), length(arguments)),
    names(arguments)
  )
  webMcpTool("t", fun, "d", arguments = arguments, ...)
}

input_schema <- function(arguments) webMcpToolInfo(tool_with(arguments), "t")$inputSchema

schema_json <- function(type) as.character(toJSON(webMcpTypeSchema(type)))

# Agent input as R receives it: JSON parsed with `simplifyVector = FALSE`
conform_json <- function(arguments, json) {
  args <- safeFromJSON(json, simplifyVector = FALSE)
  webMcpConformObject(arguments, FALSE, args, "", simplify = TRUE)
}

# A tool's return value as it is sent back: checked, then serialized
value_json <- function(type, x) {
  as.character(toJSON(webMcpConform(type, x, "value", simplify = FALSE)))
}

# Defining `arguments` ---------------------------------------------------------

test_that("`arguments` must be a named list of ellmer types", {
  f <- function(x) x
  bad <- list(
    "a type, not a list" = ellmer::type_string(),
    "NULL" = NULL,
    "unnamed" = list(ellmer::type_string()),
    "partly named" = list(x = ellmer::type_string(), ellmer::type_string()),
    "empty name" = stats::setNames(list(ellmer::type_string()), ""),
    "duplicate names" = list(x = ellmer::type_string(), x = ellmer::type_string()),
    "a string" = list(x = "string"),
    "a JSON Schema list" = list(x = list(type = "string")),
    "a function" = list(x = ellmer::type_string)
  )
  for (case in names(bad)) {
    expect_error(
      webMcpTool("t", f, "d", arguments = bad[[case]]),
      "`arguments` must be a named list of ellmer types",
      info = case
    )
  }
  expect_s3_class(webMcpTool("t", function() 1, "d", arguments = list()), "shiny_webmcp_tool")
})

test_that("`arguments` and `fun`'s formals must match", {
  s <- ellmer::type_string()
  # A type with no formal
  expect_error(webMcpTool("t", function(a) a, "d", arguments = list(a = s, b = s)), "no argument for: b")
  # A formal with no type and no default
  expect_error(webMcpTool("t", function(a, b) a, "d", arguments = list(a = s)), "no type for `fun`'s argument: b")
  # Formals with defaults don't need a type
  expect_s3_class(webMcpTool("t", function(a, b = 2) a, "d", arguments = list(a = s)), "shiny_webmcp_tool")
  # Order doesn't matter
  expect_s3_class(webMcpTool("t", function(b, a) a, "d", arguments = list(a = s, b = s)), "shiny_webmcp_tool")
  # Reserved names
  for (nm in c("input", "session")) {
    args <- stats::setNames(list(s), nm)
    fun <- function() NULL
    formals(fun) <- stats::setNames(list(quote(expr = )), nm)
    expect_error(webMcpTool("t", fun, "d", arguments = args), paste0("can't describe `", nm, "`"))
  }
  # `output` is an ordinary name
  expect_s3_class(webMcpTool("t", function(output) 1, "d", arguments = list(output = s)), "shiny_webmcp_tool")
})

test_that("unsupported ellmer types are refused when the tool is made", {
  expect_error(tool_with(list(x = ellmer::type_ignore())), "Unsupported ellmer type: ellmer::TypeIgnore")
  expect_error(
    tool_with(list(x = ellmer::type_array(ellmer::type_ignore()))),
    "Unsupported ellmer type"
  )
})

test_that("`value` must be NULL or an ellmer object type", {
  f <- function() 1
  expect_s3_class(webMcpTool("t", f, "d"), "shiny_webmcp_tool")
  expect_s3_class(webMcpTool("t", f, "d", value = ellmer::type_object()), "shiny_webmcp_tool")
  for (type in list(ellmer::type_string(), ellmer::type_array(ellmer::type_string()), list(type = "object"))) {
    expect_error(webMcpTool("t", f, "d", value = type), "`value` must be")
  }
})

# Types -> JSON Schema -------------------------------------------------------

test_that("each basic type becomes its JSON Schema type", {
  expect_equal(schema_json(ellmer::type_string()), '{"type":"string"}')
  expect_equal(schema_json(ellmer::type_number()), '{"type":"number"}')
  expect_equal(schema_json(ellmer::type_integer()), '{"type":"integer"}')
  expect_equal(schema_json(ellmer::type_boolean()), '{"type":"boolean"}')
})

test_that("enums become string enums, even with one value", {
  expect_equal(schema_json(ellmer::type_enum(c("a", "b"))), '{"type":"string","enum":["a","b"]}')
  expect_equal(schema_json(ellmer::type_enum("only")), '{"type":"string","enum":["only"]}')
})

test_that("descriptions are kept at every level", {
  type <- ellmer::type_array(
    ellmer::type_object(
      "One filter.",
      column = ellmer::type_enum(c("a", "b"), "Column."),
      value = ellmer::type_number("Value.")
    ),
    "All filters."
  )
  expect_equal(
    schema_json(type),
    paste0(
      '{"type":"array","items":{"type":"object","properties":{',
      '"column":{"type":"string","enum":["a","b"],"description":"Column."},',
      '"value":{"type":"number","description":"Value."}},',
      '"required":["column","value"],"additionalProperties":false,',
      '"description":"One filter."},"description":"All filters."}'
    )
  )
})

test_that("objects list only their required properties, and are always closed", {
  type <- ellmer::type_object(
    a = ellmer::type_string(),
    b = ellmer::type_string(required = FALSE),
    c = ellmer::type_object(d = ellmer::type_integer(required = FALSE), .required = FALSE)
  )
  schema <- webMcpTypeSchema(type)
  expect_equal(schema$required, list("a"))
  expect_false(schema$additionalProperties)
  expect_equal(schema$properties$c$required, list())
  expect_false(schema$properties$c$additionalProperties)
  # A length-1 `required` is still a JSON array
  expect_match(schema_json(ellmer::type_object(a = ellmer::type_string())), '"required":["a"]', fixed = TRUE)
  # An empty object has `{}` properties and `[]` required
  expect_equal(
    schema_json(ellmer::type_object()),
    '{"type":"object","properties":{},"required":[],"additionalProperties":false}'
  )
})

test_that("nested arrays and objects keep their shape", {
  expect_equal(
    schema_json(ellmer::type_array(ellmer::type_array(ellmer::type_integer()))),
    '{"type":"array","items":{"type":"array","items":{"type":"integer"}}}'
  )
  expect_equal(
    schema_json(ellmer::type_object(tags = ellmer::type_array(ellmer::type_string()))),
    paste0(
      '{"type":"object","properties":{"tags":{"type":"array","items":{"type":"string"}}},',
      '"required":["tags"],"additionalProperties":false}'
    )
  )
})

test_that("type_from_schema() is passed through as written", {
  json <- '{"anyOf":[{"type":"string"},{"type":"null"}],"description":"Maybe text."}'
  expect_equal(schema_json(ellmer::type_from_schema(json)), json)
})

test_that("the input schema is a closed object of the arguments", {
  schema <- input_schema(list(
    column = ellmer::type_enum(c("a", "b")),
    digits = ellmer::type_integer(required = FALSE)
  ))
  expect_equal(schema$type, "object")
  expect_named(schema$properties, c("column", "digits"))
  expect_equal(schema$required, list("column"))
  expect_false(schema$additionalProperties)

  # All optional: an empty `required` array
  schema <- input_schema(list(x = ellmer::type_string(required = FALSE)))
  expect_equal(as.character(toJSON(schema$required)), "[]")
})

# Open objects -----------------------------------------------------------------

test_that("open objects are found anywhere in `arguments`", {
  open <- function(json) webMcpOpenObjects(webMcpTypeSchema(ellmer::type_from_schema(json)), "x")

  # Open: `true`, a schema, or missing (open by default in JSON Schema)
  expect_equal(open('{"type":"object","additionalProperties":true}'), "x")
  expect_equal(open('{"type":"object","additionalProperties":{"type":"string"}}'), "x")
  expect_equal(open('{"type":"object"}'), "x")
  expect_equal(open('{"type":["object","null"]}'), "x")
  # Closed
  expect_equal(open('{"type":"object","additionalProperties":false}'), character(0))
  expect_equal(open('{"type":"string"}'), character(0))
  # Nested, with the path to each
  expect_equal(open('{"type":"array","items":{"type":"object"}}'), "x.items")
  expect_equal(
    open('{"type":"object","additionalProperties":false,"properties":{"a":{"type":"object"},"b":{"type":"object"}}}'),
    c("x.properties.a", "x.properties.b")
  )
  expect_equal(open('{"oneOf":[{"type":"string"},{"type":"object"}]}'), "x.oneOf[2]")
  # A property that happens to be named "type" is not mistaken for a type
  expect_equal(
    open('{"type":"object","additionalProperties":false,"properties":{"type":{"type":"string"}}}'),
    character(0)
  )
})

test_that("webMcpTool() refuses open objects at any depth", {
  open <- ellmer::type_from_schema('{"type":"object"}')
  cases <- list(
    top = open,
    in_array = ellmer::type_array(open),
    in_object = ellmer::type_object(inner = open),
    in_array_of_objects = ellmer::type_array(ellmer::type_object(inner = open))
  )
  for (case in names(cases)) {
    expect_error(tool_with(list(x = cases[[case]])), "can't contain open objects", info = case)
  }
  # Only the first is named in the error
  expect_error(tool_with(list(a = open, b = open)), "open objects (`a`)", fixed = TRUE)
  # Including the deprecated `type_object(.additional_properties = TRUE)`
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_error(
    tool_with(list(x = ellmer::type_object(.additional_properties = TRUE))),
    "can't contain open objects"
  )
  # Closed objects from a schema are fine
  expect_s3_class(
    tool_with(list(x = ellmer::type_from_schema('{"type":"object","additionalProperties":false}'))),
    "shiny_webmcp_tool"
  )
})

test_that("open objects are allowed in `value`", {
  expect_s3_class(
    webMcpTool(
      "t", function() list(), "d",
      value = ellmer::type_object(extra = ellmer::type_from_schema('{"type":"object"}'))
    ),
    "shiny_webmcp_tool"
  )
})

# Checking agent input -------------------------------------------------------

test_that("scalars are checked and converted", {
  args <- list(
    s = ellmer::type_string(),
    n = ellmer::type_number(),
    i = ellmer::type_integer(),
    b = ellmer::type_boolean(),
    e = ellmer::type_enum(c("x", "y"))
  )
  out <- conform_json(args, '{"s":"hi","n":1.5,"i":3,"b":true,"e":"y"}')
  expect_identical(out, list(s = "hi", n = 1.5, i = 3L, b = TRUE, e = "y"))

  # Whole numbers are integers whether or not JSON wrote a decimal point
  expect_identical(conform_json(args["i"], '{"i":3.0}')$i, 3L)
  # Integers are numbers
  expect_identical(conform_json(args["n"], '{"n":2}')$n, 2L)
})

test_that("scalars of the wrong kind are refused, naming the argument", {
  cases <- list(
    s = c('{"s":1}', '{"s":true}', '{"s":null}', '{"s":["a"]}', '{"s":{"a":1}}'),
    n = c('{"n":"1"}', '{"n":true}', '{"n":[1]}'),
    i = c('{"i":"3"}', '{"i":true}'),
    b = c('{"b":1}', '{"b":"true"}', '{"b":[true]}'),
    e = c('{"e":1}', '{"e":["x"]}')
  )
  args <- list(
    s = ellmer::type_string(),
    n = ellmer::type_number(),
    i = ellmer::type_integer(),
    b = ellmer::type_boolean(),
    e = ellmer::type_enum(c("x", "y"))
  )
  for (arg in names(cases)) {
    for (json in cases[[arg]]) {
      expect_error(
        conform_json(args[arg], json),
        paste0("`", arg, "`"),
        info = json
      )
    }
  }
  expect_error(conform_json(args["i"], '{"i":1.5}'), "`i` must be an integer; got 1.5")
  # Beyond R's integer range, rather than becoming NA
  expect_error(conform_json(args["i"], '{"i":3e10}'), "`i` must be an integer")
  expect_error(conform_json(args["e"], '{"e":"z"}'), '`e` must be one of "x", "y"; got "z"')
})

test_that("required and optional arguments", {
  args <- list(a = ellmer::type_string(), b = ellmer::type_string(required = FALSE))
  expect_identical(conform_json(args, '{"a":"1"}'), list(a = "1"))
  # `null` is the same as leaving it out
  expect_identical(conform_json(args, '{"a":"1","b":null}'), list(a = "1"))
  expect_error(conform_json(args, '{"b":"2"}'), "Missing required parameter `a`")
  expect_error(conform_json(args, '{"a":null}'), "Missing required parameter `a`")
  expect_error(conform_json(args, "{}"), "Missing required parameter `a`")
  expect_error(conform_json(args, '{"a":"1","c":3}'), "Unknown parameter `c`")
})

test_that("the input must be an object", {
  args <- list(a = ellmer::type_string(required = FALSE))
  expect_identical(conform_json(args, "{}"), stats::setNames(list(), character(0)))
  expect_error(conform_json(args, "[1]"), "Input must be an object")
  expect_error(conform_json(args, '"a"'), "Input must be an object")
})

test_that("arrays of scalars become atomic vectors", {
  arr <- function(type) list(x = ellmer::type_array(type))
  expect_identical(conform_json(arr(ellmer::type_string()), '{"x":["a","b"]}')$x, c("a", "b"))
  expect_identical(conform_json(arr(ellmer::type_number()), '{"x":[1.5,2]}')$x, c(1.5, 2))
  expect_identical(conform_json(arr(ellmer::type_integer()), '{"x":[1,2.0]}')$x, c(1L, 2L))
  expect_identical(conform_json(arr(ellmer::type_boolean()), '{"x":[true,false]}')$x, c(TRUE, FALSE))
  expect_identical(conform_json(arr(ellmer::type_enum(c("a", "b"))), '{"x":["b"]}')$x, "b")

  # Empty arrays keep their type
  expect_identical(conform_json(arr(ellmer::type_string()), '{"x":[]}')$x, character(0))
  expect_identical(conform_json(arr(ellmer::type_number()), '{"x":[]}')$x, numeric(0))
  expect_identical(conform_json(arr(ellmer::type_integer()), '{"x":[]}')$x, integer(0))
  expect_identical(conform_json(arr(ellmer::type_boolean()), '{"x":[]}')$x, logical(0))
  expect_identical(conform_json(arr(ellmer::type_enum("a")), '{"x":[]}')$x, character(0))
})

test_that("arrays of arrays and of objects stay lists", {
  nested <- list(x = ellmer::type_array(ellmer::type_array(ellmer::type_integer())))
  expect_identical(conform_json(nested, '{"x":[[1,2],[],[3]]}')$x, list(c(1L, 2L), integer(0), 3L))

  pairs <- list(x = ellmer::type_array(ellmer::type_object(
    column = ellmer::type_enum(c("cyl", "gear")),
    value = ellmer::type_number()
  )))
  expect_identical(
    conform_json(pairs, '{"x":[{"column":"cyl","value":6},{"column":"gear","value":4.5}]}')$x,
    list(list(column = "cyl", value = 6L), list(column = "gear", value = 4.5))
  )
  expect_identical(conform_json(pairs, '{"x":[]}')$x, list())
})

test_that("array errors name the item", {
  arr <- list(x = ellmer::type_array(ellmer::type_number()))
  expect_error(conform_json(arr, '{"x":[1,"2"]}'), "`x[2]` must be a number", fixed = TRUE)
  expect_error(conform_json(arr, '{"x":1}'), NA) # a lone number is a length-1 array in R
  expect_error(conform_json(arr, '{"x":"a"}'), "`x[1]` must be a number", fixed = TRUE)
  expect_error(conform_json(arr, '{"x":{"a":1}}'), "`x` must be an array")

  pairs <- list(x = ellmer::type_array(ellmer::type_object(
    column = ellmer::type_enum(c("cyl", "gear")),
    value = ellmer::type_number()
  )))
  expect_error(
    conform_json(pairs, '{"x":[{"column":"cyl","value":6},{"column":"hp","value":1}]}'),
    "`x[2].column` must be one of",
    fixed = TRUE
  )
  expect_error(
    conform_json(pairs, '{"x":[{"column":"cyl"}]}'),
    "Missing required parameter `x[1].value`",
    fixed = TRUE
  )
  expect_error(
    conform_json(pairs, '{"x":[{"column":"cyl","value":1,"op":"<"}]}'),
    "Unknown parameter `x[1].op`",
    fixed = TRUE
  )
  expect_error(conform_json(pairs, '{"x":[1]}'), "`x[1]` must be an object", fixed = TRUE)
})

test_that("nested objects are checked with dotted paths", {
  args <- list(range = ellmer::type_object(
    from = ellmer::type_number(),
    to = ellmer::type_number(required = FALSE),
    unit = ellmer::type_object(name = ellmer::type_string())
  ))
  expect_identical(
    conform_json(args, '{"range":{"from":1,"unit":{"name":"s"}}}')$range,
    list(from = 1L, unit = list(name = "s"))
  )
  expect_error(conform_json(args, '{"range":{"unit":{"name":"s"}}}'), "Missing required parameter `range.from`")
  expect_error(conform_json(args, '{"range":{"from":1,"unit":{}}}'), "Missing required parameter `range.unit.name`")
  expect_error(conform_json(args, '{"range":{"from":1,"unit":{"name":1}}}'), "`range.unit.name` must be a string")
  expect_error(conform_json(args, '{"range":{"from":1,"unit":{"name":"s"},"by":2}}'), "Unknown parameter `range.by`")
  expect_error(conform_json(args, '{"range":[1,2]}'), "`range` must be an object")
})

test_that("type_from_schema() values are passed through unchecked", {
  args <- list(x = ellmer::type_from_schema('{"type":"string"}'))
  expect_identical(conform_json(args, '{"x":"a"}')$x, "a")
  expect_identical(conform_json(args, '{"x":[1,2]}')$x, list(1L, 2L))
})

# Checking the return value ----------------------------------------------------

test_that("return values are checked and serialized with their shape", {
  value <- ellmer::type_object(
    n = ellmer::type_integer(),
    mean = ellmer::type_number(),
    tags = ellmer::type_array(ellmer::type_string()),
    note = ellmer::type_string(required = FALSE)
  )
  # A length-1 array is still an array; integer-valued doubles become integers
  expect_equal(
    value_json(value, list(n = 3, mean = 1.5, tags = "a")),
    '{"n":3,"mean":1.5,"tags":["a"]}'
  )
  expect_equal(
    value_json(value, list(n = 3L, mean = 2, tags = c("a", "b"), note = "x")),
    '{"n":3,"mean":2,"tags":["a","b"],"note":"x"}'
  )
  # Empty arrays too
  expect_equal(value_json(value, list(n = 0, mean = 0, tags = character(0))), '{"n":0,"mean":0,"tags":[]}')

  expect_error(value_json(value, list(n = 1.5, mean = 1, tags = "a")), "`value.n` must be an integer")
  expect_error(value_json(value, list(n = 1, mean = 1, tags = 1)), "`value.tags[1]` must be a string", fixed = TRUE)
  expect_error(value_json(value, list(n = 1, mean = 1)), "Missing required parameter `value.tags`")
  expect_error(value_json(value, list(n = 1, mean = 1, tags = "a", extra = 1)), "Unknown parameter `value.extra`")
  expect_error(value_json(value, list(1, 1, "a")), "`value` must be an object")
  expect_error(value_json(value, "text"), "`value` must be an object")
  expect_error(value_json(value, list(n = NA, mean = 1, tags = "a")), "`value.n` must be an integer")
  expect_error(value_json(value, list(n = 1:2, mean = 1, tags = "a")), "`value.n` must be an integer")
})

test_that("arrays in return values can't be matrices or named lists", {
  value <- ellmer::type_object(x = ellmer::type_array(ellmer::type_number()))
  expect_error(value_json(value, list(x = matrix(1:4, 2))), "`value.x` must be an array")
  expect_error(value_json(value, list(x = list(a = 1))), "`value.x` must be an array")
  expect_equal(value_json(value, list(x = list(1, 2))), '{"x":[1,2]}')
})

test_that("arrays of objects in return values stay arrays of objects", {
  value <- ellmer::type_object(rows = ellmer::type_array(ellmer::type_object(
    name = ellmer::type_string(),
    n = ellmer::type_integer()
  )))
  expect_equal(
    value_json(value, list(rows = list(list(name = "a", n = 1)))),
    '{"rows":[{"name":"a","n":1}]}'
  )
  expect_error(
    value_json(value, list(rows = list(list(name = "a")))),
    "Missing required parameter `value.rows[1].n`",
    fixed = TRUE
  )
})
