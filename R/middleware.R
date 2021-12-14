# This file contains a general toolkit for routing and combining bits of
# HTTP-handling logic. It is similar in spirit to Rook (and Rack, and WSGI, and
# Connect, and...) but adds cascading and routing.
#
# This file is called "middleware" because that's the term used for these bits
# of logic in these other frameworks. However, our code uses the word "handler"
# so we'll stick to that for the rest of this document; just know that they're
# basically the same concept.
#
# ## Intro to handlers
#
# A **handler** (or sometimes, **httpHandler**) is a function that takes a
# `req` parameter--a request object as described in the Rook specification--and
# returns `NULL`, or an `httpResponse`.
#
## ------------------------------------------------------------------------

#' Create an HTTP response object
#' @description
#' One can use it together with e.g `session$registerDataObj` (exposing REST API endpoints) or `ui` function in Shiny (see examples).
#'
#' @param status [HTTP status code](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status) for the response.
#' @param content_type The value for the `Content-Type` header. For example: `text/plain`, `application/json`, `text/csv` [(see more examples)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types#important_mime_types_for_web_developers)
#' @param content The body of the response, given as a single-element character
#'   vector (will be encoded as UTF-8) or a raw vector.
#' @param headers A named list of additional headers to include. Do not include
#'   `Content-Length` (as it is automatically calculated) or `Content-Type` (the
#'   `content_type` argument is used instead).
#'
#' @examples
#'#' Example 1
#'#' In this example we are exposing REST API endpoint where the httpResponse returns `svg-xml` ggplot object
#'
#' library(shiny)
#' library(ggplot2)
#' library(svg)
#'
#' example_gg_plot <- ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
#'
#' ui <- fluidPage(
#'   div(id = "svg_ggplot")
#' )
#'
#' server <- function(input, output, session) {
#'   #' We create REST API endpoint for fetching ggplot (as svg)
#'   #' Looks similarily to `session/13b6edsessiontoken3764158e8a3af1/dataobj/example_plot_svg?w=&nonce=14367c50429fc201`
#'   ggplot_rest_url <- session$registerDataObj(
#'     name = "example_plot_svg",
#'     data = list(),
#'     filterFunc = function(data, req) {
#'       if(identical(req$REQUEST_METHOD, "GET")) {
#'         tmp_path <- paste0(tempfile(),".svg")
#'         ggsave(tmp_path, plot = example_gg_plot)
#'         raw_gg <- readBin(tmp_path, "raw", 100000000)
#'         httpResponse(
#'           status = 200,
#'           content_type = "image/svg+xml",
#'           content = raw_gg
#'         )
#'       }
#'     }
#'   )
#'
#'   # The plot is then exposed as `svg` through httpResponse within `img` tag
#'   insertUI(
#'     selector = "#svg_ggplot",
#'     ui = tags$img(src = ggplot_rest_url)
#'   )
#' }
#'
#' shinyApp(ui, server)
#'
#'
#'#' Example 2
#'#' In this example we are exposing REST API endpoint which can be used for sending
#'#' POST requests. The request is handled and then the `httpResponse` is sending the response
#'
#' library(shiny)
#' library(jsonlite)
#'
#' js_code_communication <- '
#'   let post_url = "";
#'   const submit = async function() {
#'     const request_body = {
#'       first_name: $("#first_name").val(),
#'       last_name: $("#last_name").val()
#'     }
#'
#'     $("#post_request_body").text(JSON.stringify(request_body));
#'
#'     const response = await fetch(post_url, {
#'       method: "POST",
#'       body: JSON.stringify(request_body)
#'     }).then(function(response) {
#'       return response.json();
#'     });
#'
#'     $("#post_response_body").text(JSON.stringify(response));
#'   };
#'
#'   Shiny.addCustomMessageHandler("submit_endpoint_url", function(url) {
#'     post_url = url;
#'   })
#' '
#'
#' ui <- function() {
#'   fluidPage(
#'     textInput("first_name", "First name:"),
#'     textInput("last_name", "Last name:"),
#'     actionButton("submit", "Submit", onclick="submit()"),
#'     tags$script(js_code_communication),
#'     tags$p("POST REQUEST BODY"),
#'     div(id = "post_request_body"),
#'     tags$p("POST RESPONSE BODY"),
#'     div(id = "post_response_body")
#'   )
#' }
#'
#' server <- function(input, output, session) {
#'   #' We create REST API session-scoped endpoint for making POST requests
#'   #' Looks similarily to `session/13b6edsessiontoken3764158e8a3af1/dataobj/example_post_url?w=&nonce=14367c50429fc201`
#'   submit_endpoint_url <- session$registerDataObj(
#'     name = "example_post_url",
#'     data = list(),
#'     filterFunc = function(data, req) {
#'       if(identical(req$REQUEST_METHOD, "POST")) {
#'         received = req$rook.input$read_lines() %>% fromJSON()
#'         print(received)
#'         response <- append(received, list(server_message = "HELLO"))
#'         httpResponse(
#'           status = 200,
#'           content_type = "application/json",
#'           content = toJSON(response, auto_unbox = TRUE)
#'         )
#'       }
#'     }
#'   )
#'
#'   #' We have to let the browser know what is the session-scoped endpoint for POST requests
#'   session$sendCustomMessage("submit_endpoint_url", submit_endpoint_url)
#' }
#'
#' shinyApp(ui, server)
#'
#'
#'#' Example 3
#'#' One can use it to serve requests from `ui` function.
#'Please note additional argument - `req` - which is a Rook environment
#'You can read more here: https://github.com/jeffreyhorner/Rook#the-environment
#' #'
#' library(shiny)
#' library(jsonlite)
#'
#' ui <- function(req) {
#'   if (identical(req$REQUEST_METHOD, "GET")) {
#'     fluidPage(
#'       tags$p("Usual UI"),
#'       markdown(
#'         sprintf("
#'         You can now check it out by sending POST request.
#'         You could do it by running the following command in browser console (the app must be running first):
#'         ```
#'         await fetch('http://%s', {
#'            method: 'POST',
#'            body: JSON.stringify({
#'              my_value: \"23\",
#'              my_array: [1,2,3,4]
#'            })
#'          }).then((response) => response.json())
#'          ```
#'       ", req$HTTP_HOST))
#'     )
#'   } else if (identical(req$REQUEST_METHOD, "POST")) {
#'     received <- fromJSON(req$rook.input$read_lines())
#'     print(received)
#'     # Be sure to return a response
#'     httpResponse(
#'       status = 200L,
#'       content_type = "application/json",
#'       content = toJSON(received),
#'       headers = list(
#'         `Access-Control-Allow-Origin` = "*" # Added to allow requests from other hosts, not necessary if used from the same host
#'       )
#'     )
#'   }
#' }
#'
#' attr(ui, "http_methods_supported") <- c("GET", "POST")
#'
#' server <- function(input, output, session) {
#'   #' Please note, that in this example, when sending a POST request from various places, there is no new session initialized
#'   print(session$token)
#' }
#'
#' shinyApp(ui, server)
#'
#' @keywords internal
#' @export
httpResponse <- function(status = 200L,
                         content_type = "text/html; charset=UTF-8",
                         content = "",
                         headers = list()) {
  # Make sure it's a list, not a vector
  headers <- as.list(headers)
  if (is.null(headers$`X-UA-Compatible`))
    headers$`X-UA-Compatible` <- "IE=edge,chrome=1"
  resp <- list(status = status, content_type = content_type, content = content,
               headers = headers)
  class(resp) <- 'httpResponse'
  return(resp)
}

#
# You can think of a web application as being simply an aggregation of these
# functions, each of which performs one kind of duty. Each handler in turn gets
# a look at the request and can decide whether it knows how to handle it. If
# so, it returns an `httpResponse` and processing terminates; if not, it
# returns `NULL` and the next handler gets to execute. If the final handler
# returns `NULL`, a 404 response should be returned.
#
# We have a similar construct for websockets: **websocket handlers** or
# **wsHandlers**. These take a single `ws` argument which is the websocket
# connection that was just opened, and they can either return `TRUE` if they
# are handling the connection, and `NULL` to pass responsibility on to the next
# wsHandler.
#
# ### Combining handlers
#
# Since it's so common for httpHandlers to be invoked in this "cascading"
# fashion, we'll introduce a function that takes zero or more handlers and
# returns a single handler. And while we're at it, making a directory of static
# content available is such a common thing to do, we'll allow strings
# representing paths to be used instead of handlers; any such strings we
# encounter will be converted into `staticHandler` objects.
#
## ------------------------------------------------------------------------
joinHandlers <- function(handlers) {
  # Zero handlers; return a null handler
  if (length(handlers) == 0)
    return(function(req) NULL)

  # Just one handler (function)? Return it.
  if (is.function(handlers))
    return(handlers)

  handlers <- lapply(handlers, function(h) {
    if (is.character(h))
      return(staticHandler(h))
    else
      return(h)
  })

  # Filter out NULL
  handlers <- handlers[!sapply(handlers, is.null)]

  if (length(handlers) == 0)
    return(function(req) NULL)
  if (length(handlers) == 1)
    return(handlers[[1]])

  function(req) {
    for (handler in handlers) {
      response <- handler(req)
      if (!is.null(response))
        return(response)
    }
    return(NULL)
  }
}

#
# Note that we don't have an equivalent of `joinHandlers` for wsHandlers. It's
# easy to imagine it, we just haven't needed one.
#
# ### Handler routing
#
# Handlers do not have a built-in notion of routing. Conceptually, given a list
# of handlers, all the handlers are peers and they all get to see every request
# (well, up until the point that a handler returns a response).
#
# You could implement routing in each handler by checking the request's
# `PATH_INFO` field, but since it's such a common need, let's make it simple by
# introducing a `routeHandler` function. This is a handler
# [decorator](http://en.wikipedia.org/wiki/Decorator_pattern) and it's
# responsible for 1) filtering out requests that don't match the given route,
# and 2) temporarily modifying the request object to take the matched part of
# the route off of the `PATH_INFO` (and add it to the end of `SCRIPT_NAME`).
# This way, the handler doesn't need to figure out about what part of its URL
# path has already been matched via routing.
#
# (BTW, it's safe for `routeHandler` calls to nest.)
#
## ------------------------------------------------------------------------
routeHandler <- function(prefix, handler) {
  force(prefix)
  force(handler)

  if (identical("", prefix))
    return(handler)

  if (length(prefix) != 1 || !isTRUE(grepl("^/[^\\]+$", prefix))) {
    stop("Invalid URL prefix \"", prefix, "\"")
  }

  pathPattern <- paste("^\\Q", prefix, "\\E/", sep = "")
  function(req) {
    if (isTRUE(grepl(pathPattern, req$PATH_INFO))) {
      origScript <- req$SCRIPT_NAME
      origPath <- req$PATH_INFO
      on.exit({
        req$SCRIPT_NAME <- origScript
        req$PATH_INFO <- origPath
      }, add = TRUE)
      pathInfo <- substr(req$PATH_INFO, nchar(prefix)+1, nchar(req$PATH_INFO))
      req$SCRIPT_NAME <- paste(req$SCRIPT_NAME, prefix, sep = "")
      req$PATH_INFO <- pathInfo
      return(handler(req))
    } else {
      return(NULL)
    }
  }
}

#
# We have a version for websocket handlers as well. Pity about the copy/paste
# job.
#
## ------------------------------------------------------------------------
routeWSHandler <- function(prefix, wshandler) {
  force(prefix)
  force(wshandler)

  if (identical("", prefix))
    return(wshandler)

  if (length(prefix) != 1 || !isTRUE(grepl("^/[^\\]+$", prefix))) {
    stop("Invalid URL prefix \"", prefix, "\"")
  }

  pathPattern <- paste("^\\Q", prefix, "\\E/", sep = "")
  function(ws) {
    req <- ws$request
    if (isTRUE(grepl(pathPattern, req$PATH_INFO))) {
      origScript <- req$SCRIPT_NAME
      origPath <- req$PATH_INFO
      on.exit({
        req$SCRIPT_NAME <- origScript
        req$PATH_INFO <- origPath
      }, add = TRUE)
      pathInfo <- substr(req$PATH_INFO, nchar(prefix)+1, nchar(req$PATH_INFO))
      req$SCRIPT_NAME <- paste(req$SCRIPT_NAME, prefix, sep = "")
      req$PATH_INFO <- pathInfo
      return(wshandler(ws))
    } else {
      return(NULL)
    }
  }
}

#
# ### Handler implementations
#
# Now let's actually write some handlers. Note that these functions aren't
# *themselves* handlers, you call them and they *return* a handler. Handler
# factory functions, if you will.
#
# Here's one that serves up static assets from a directory.
#
## ------------------------------------------------------------------------
staticHandler <- function(root) {
  force(root)
  return(function(req) {
    if (!identical(req$REQUEST_METHOD, 'GET'))
      return(NULL)

    path <- URLdecode(req$PATH_INFO)

    if (is.null(path))
      return(httpResponse(400, content="<h1>Bad Request</h1>"))

    if (path == '/')
      path <- '/index.html'

    if (grepl('\\', path, fixed = TRUE))
      return(NULL)

    abs.path <- resolve(root, path)
    if (is.null(abs.path))
      return(NULL)

    content.type <- getContentType(abs.path)
    response.content <- readBin(abs.path, 'raw', n=file.info(abs.path)$size)
    return(httpResponse(200, content.type, response.content))
  })
}

#
# ## Handler manager
#
# The handler manager gives you a place to register handlers (of both http and
# websocket varieties) and provides an httpuv-compatible set of callbacks for
# invoking them.
#
# Create one of these, make zero or more calls to `addHandler` and
# `addWSHandler` methods (order matters--first one wins!), and then pass the
# return value of `createHttpuvApp` to httpuv's `startServer` function.
#
## ------------------------------------------------------------------------
HandlerList <- R6Class("HandlerList",
  portable = FALSE,
  class = FALSE,
  public = list(
    handlers = list(),

    add = function(handler, key, tail = FALSE) {
      if (!is.null(handlers[[key]]))
        stop("Key ", key, " already in use")
      newList <- structure(names=key, list(handler))

      if (length(handlers) == 0)
        handlers <<- newList
      else if (tail)
        handlers <<- c(handlers, newList)
      else
        handlers <<- c(newList, handlers)
    },
    remove = function(key) {
      handlers[key] <<- NULL
    },
    clear = function() {
      handlers <<- list()
    },
    invoke = function(...) {
      for (handler in handlers) {
        result <- handler(...)
        if (!is.null(result))
          return(result)
      }
      return(NULL)
    }
  )
)

HandlerManager <- R6Class("HandlerManager",
  portable = FALSE,
  class = FALSE,
  public = list(
    handlers = "HandlerList",
    wsHandlers = "HandlerList",

    initialize = function() {
      handlers <<- HandlerList$new()
      wsHandlers <<- HandlerList$new()
    },

    addHandler = function(handler, key, tail = FALSE) {
      handlers$add(handler, key, tail)
    },
    removeHandler = function(key) {
      handlers$remove(key)
    },
    addWSHandler = function(wsHandler, key, tail = FALSE) {
      wsHandlers$add(wsHandler, key, tail)
    },
    removeWSHandler = function(key) {
      wsHandlers$remove(key)
    },
    clear = function() {
      handlers$clear()
      wsHandlers$clear()
    },
    createHttpuvApp = function() {
      list(
        onHeaders = function(req) {
          maxSize <- getOption('shiny.maxRequestSize') %||% (5 * 1024 * 1024)
          if (maxSize <= 0)
            return(NULL)

          reqSize <- 0
          if (length(req$CONTENT_LENGTH) > 0)
            reqSize <- as.numeric(req$CONTENT_LENGTH)
          else if (length(req$HTTP_TRANSFER_ENCODING) > 0)
            reqSize <- Inf

          if (reqSize > maxSize) {
            return(list(status = 413L,
              headers = list('Content-Type' = 'text/plain'),
              body = 'Maximum upload size exceeded'))
          }
          else {
            return(NULL)
          }
        },
        call = .httpServer(
          function (req) {
            hybrid_chain(
              hybrid_chain(
                withCallingHandlers(withLogErrors(handlers$invoke(req)),
                  error = function(cond) {
                    sanitizeErrors <- getOption('shiny.sanitize.errors', FALSE)
                    if (inherits(cond, 'shiny.custom.error') || !sanitizeErrors) {
                      stop(cond$message, call. = FALSE)
                    } else {
                      stop(paste("An error has occurred. Check your logs or",
                                 "contact the app author for clarification."),
                           call. = FALSE)
                    }
                  }
                ),
                catch = function(err) {
                  httpResponse(status = 500L,
                    content_type = "text/html; charset=UTF-8",
                    content = as.character(htmltools::htmlTemplate(
                      system_file("template", "error.html", package = "shiny"),
                      message = conditionMessage(err)
                    ))
                  )
                }
              ),
              function(resp) {
                maybeInjectAutoreload(resp)
              }
            )
          },
          loadSharedSecret()
        ),
        onWSOpen = function(ws) {
          return(wsHandlers$invoke(ws))
        }
      )
    },
    .httpServer = function(handler, checkSharedSecret) {
      filter <- getOption('shiny.http.response.filter')
      if (is.null(filter))
        filter <- function(req, response) response

      function(req) {
        if (!checkSharedSecret(req$HTTP_SHINY_SHARED_SECRET)) {
          return(list(status=403,
            body='<h1>403 Forbidden</h1><p>Shared secret mismatch</p>',
            headers=list('Content-Type' = 'text/html')))
        }

        # Catch HEAD requests. For the purposes of handler functions, they
        # should be treated like GET. The difference is that they shouldn't
        # return a body in the http response.
        head_request <- FALSE
        if (identical(req$REQUEST_METHOD, "HEAD")) {
          head_request <- TRUE
          req$REQUEST_METHOD <- "GET"
        }

        response <- handler(req)

        res <- hybrid_chain(response, function(response) {
          if (is.null(response))
            response <- httpResponse(404, content="<h1>Not Found</h1>")

          if (inherits(response, "httpResponse")) {
            headers <- as.list(response$headers)
            headers$'Content-Type' <- response$content_type

            response <- filter(req, response)
            if (head_request) {

              headers$`Content-Length` <- getResponseContentLength(response, deleteOwnedContent = TRUE)

              return(list(
                status = response$status,
                body = "",
                headers = headers
              ))
            } else {
              return(list(
                status = response$status,
                body = response$content,
                headers = headers
              ))
            }

          } else {
            # Assume it's a Rook-compatible response
            return(response)
          }
        })
      }
    }
  )
)

maybeInjectAutoreload <- function(resp) {
  if (get_devmode_option("shiny.autoreload", FALSE) &&
      isTRUE(grepl("^text/html($|;)", resp$content_type)) &&
      is.character(resp$content)) {

    resp$content <- gsub(
      "</head>",
      "<script src=\"shared/shiny-autoreload.js\"></script>\n</head>",
      resp$content,
      fixed = TRUE
    )
  }

  resp
}

# Safely get the Content-Length of a Rook response, or NULL if the length cannot
# be determined for whatever reason (probably malformed response$content).
# If deleteOwnedContent is TRUE, then the function should delete response
# content that is of the form list(file=..., owned=TRUE).
getResponseContentLength <- function(response, deleteOwnedContent) {
  force(deleteOwnedContent)

  result <- if (is.character(response$content) && length(response$content) == 1) {
    nchar(response$content, type = "bytes")
  } else if (is.raw(response$content)) {
    length(response$content)
  } else if (is.list(response$content) && !is.null(response$content$file)) {
    if (deleteOwnedContent && isTRUE(response$content$owned)) {
      on.exit(unlink(response$content$file, recursive = FALSE, force = FALSE), add = TRUE)
    }
    file.info(response$content$file)$size
  } else {
    warning("HEAD request for unexpected content class ", class(response$content)[[1]])
    NULL
  }

  if (is.na(result)) {
    # Mostly for missing file case
    return(NULL)
  } else {
    return(result)
  }
}

#
# ## Next steps
#
# See server.R and middleware-shiny.R to see actual implementation and usage of
# handlers in the context of Shiny.
