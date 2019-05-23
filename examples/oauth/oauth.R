# remotes::install_github("r-lib/fastmap")

# Include in your Shiny UI wherever you want OAuth login UI to appear
oauth_login_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("container"))
}

# A simple Bootstrap OAuth login button
oauth_button <- function(login_url) {
  #tags$a(href=login_url, target="_blank", class="btn btn-default", "Login")
  tags$a(href=sprintf("javascript:window.open('%s');", login_url), class = "btn btn-default", "Login")
}


# Server module for initializing oauth
oauth_login <- function(input, output, session,
  oauth_endpoint_uri, token_endpoint_uri, app_uri,
  client_id, client_secret, scope,
  login_ui = oauth_button) {

  # Force argument evaluation
  list(oauth_endpoint_uri, token_endpoint_uri, app_uri, client_id, client_secret, scope, login_ui)

  token <- reactiveVal(NULL)

  # TODO: make parsing robust (escaping)
  cookie <- session$request$HTTP_COOKIE
  if (!is.null(cookie)) {
    m <- regmatches(cookie, regexec("shinyoauthaccesstoken=([^;]+)", cookie, perl = TRUE))[[1]]
    if (length(m) > 0) {
      token(m[[2]])
    }
  }

  redirect_uri <- sub("/?$", "/oauth_callback", app_uri)

  state <- store_oauth_request_state(token, redirect_uri, token_endpoint_uri, client_id, client_secret, session)

  output$container <- renderUI({
    if (is.null(token())) {
      # login button
      url <- make_authorization_url(oauth_endpoint_uri, redirect_uri, client_id, client_secret, scope, state, session)
      login_ui(url)
    }
  })

  return(token)
}

oauth_request_state <- fastmap::fastmap()

store_oauth_request_state <- function(rv, redirect_uri, token_endpoint_uri, client_id, client_secret, session = getDefaultReactiveDomain()) {
  state <- shiny:::createUniqueId(16)
  oauth_request_state$set(state, list(
    rv = rv,
    redirect_uri = redirect_uri,
    token_endpoint_uri = token_endpoint_uri,
    client_id = client_id,
    client_secret = client_secret
  ))

  # In case the session ends, clean out the state so we don't leak memory
  shiny::onSessionEnded(function() {
    oauth_request_state$remove(state)
  })

  state
}


make_authorization_url <- function(oauth_endpoint_uri, redirect_uri, client_id, client_secret, scope, state, session = getDefaultReactiveDomain()) {
  # TODO: Implement for real
  #
  # The req object is a Rook request. This is just an environment object that
  # gives you access to the request URL, HTTP headers, etc. The documentation
  # for this object is here:
  # https://github.com/jeffreyhorner/Rook#the-environment
  url_template <- "%s?client_id=%s&redirect_uri=%s&response_type=code&state=%s&access_type=offline&include_granted_scopes=true&scope=%s"
  auth_url <- sprintf(url_template,
    oauth_endpoint_uri,
    utils::URLencode(client_id, reserved = TRUE, repeated = TRUE),
    utils::URLencode(redirect_uri, reserved = TRUE, repeated = TRUE),
    utils::URLencode(state, reserved = TRUE, repeated = TRUE),
    utils::URLencode(scope, reserved = TRUE, repeated = TRUE)
  )

  addRouteHandler("/oauth_callback", oauth_callback_handler)

  auth_url
}

# This is the Rook handler that is invoked when the browser returns
# from authenticating with the OAuth provider. Based on the `code`
# and `state` in the query string, we'll look up oauth_request_state
# and retrieve the oauth token.
oauth_callback_handler <- function(req) {
  if (!identical(req$REQUEST_METHOD, 'GET'))
    return(NULL)

  qs_info <- parseQueryString(req$QUERY_STRING)
  err <- qs_info$error
  code <- qs_info$code

  # TODO: state should be signed/verified
  state <- qs_info$state
  if (!is.null(err)) {
    # TODO: Report error to user
    message(err)
    return(NULL)
  } else if (!is.null(code) && !is.null(state)) {
    req_info <- oauth_request_state$get(state)
    if (is.null(req_info)) {
      # TODO: Report error to user
      stop("OAuth authentication request not recognized")
    }

    redirect_uri <- req_info$redirect_uri
    token_endpoint_uri <- req_info$token_endpoint_uri
    client_id <- req_info$client_id
    client_secret <- req_info$client_secret
    rv <- req_info$rv

    resp <- httr::POST(token_endpoint_uri,
      body = list(
        client_id = client_id,
        code = code,
        redirect_uri = redirect_uri,
        grant_type = "authorization_code",
        client_secret = client_secret
      )
    )
    respObj <- httr::content(resp, as = "parsed")
    # respObj <- jsonlite::fromJSON(rawToChar(resp$content))

    rv(respObj$access_token)

    return(list(
      status = 200L,
      headers = list(
        "Content-Type" = "text/html",
        # TODO: encrypt
        # TODO: expiration
        # TODO: secure (optionally)
        # TODO: escaping
        # TODO: path/samesite
        "Set-Cookie" = sprintf("shinyoauthaccesstoken=%s; HttpOnly; Path=/", respObj$access_token)
      ),
      body = "#<html><head><script>window.close();</script></head><body>You can close this window now</body></html>"
    ))
  } else {
    # TODO: Report malformed request
  }
}
