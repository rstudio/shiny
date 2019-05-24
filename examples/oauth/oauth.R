# remotes::install_github("r-lib/fastmap")

# Include in your Shiny UI wherever you want OAuth login UI to appear
oauth_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("container")),
    htmltools::singleton(tags$head(clear_cookie_custom_handler))
  )
}

# A simple Bootstrap OAuth login button
oauth_login_button <- function(login_url) {
  #tags$a(href=login_url, target="_blank", class="btn btn-default", "Login")
  tags$a(href=sprintf("javascript:window.open('%s');", login_url), class = "btn btn-default", "Login")
}

oauth_logout_button <- function(input_id) {
  actionLink(input_id, "Logout")
}

oauth_config <- function(oauth_endpoint_uri, token_endpoint_uri, app_uri,
  client_id, client_secret, scope, login_ui = oauth_login_button,
  logout_ui = oauth_logout_button) {

  list(
    oauth_endpoint_uri = oauth_endpoint_uri,
    token_endpoint_uri = token_endpoint_uri,
    app_uri = app_uri,
    client_id = client_id,
    client_secret = client_secret,
    scope = scope,
    login_ui = login_ui,
    logout_ui = logout_ui
  )
}

# Server module for initializing oauth
oauth_login <- function(input, output, session, oauth_config) {

  force(oauth_config)

  token <- reactiveVal(NULL)

  # TODO: make parsing robust (escaping)
  cookie <- session$request$HTTP_COOKIE
  if (!is.null(cookie)) {
    m <- regmatches(cookie, regexec("shinyoauthaccesstoken=([^;]+)", cookie, perl = TRUE))[[1]]
    if (length(m) > 0) {
      token(m[[2]])
    }
  }

  redirect_uri <- sub("/?$", "/oauth_callback", oauth_config$app_uri)

  state <- store_oauth_request_state(token,
    redirect_uri,
    oauth_config$token_endpoint_uri,
    oauth_config$client_id,
    oauth_config$client_secret,
    session)

  output$container <- renderUI({
    if (is.null(token())) {
      # login button
      url <- make_authorization_url(oauth_config, redirect_uri, state, session)

      oauth_config$login_ui(url)
    } else {
      oauth_config$logout_ui(session$ns("btn_logout"))
    }
  })

  observeEvent(input$btn_logout, {
    xsrf_token <- shiny:::createUniqueId(16)
    clear_cookie_xsrf$set(xsrf_token, TRUE)

    session$sendCustomMessage("oauth-clear-cookie-handler", list(
      xsrf_token = xsrf_token
    ))
    token(NULL)
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


make_authorization_url <- function(oauth_config, redirect_uri, state, session = getDefaultReactiveDomain()) {
  # TODO: Implement for real
  #
  # The req object is a Rook request. This is just an environment object that
  # gives you access to the request URL, HTTP headers, etc. The documentation
  # for this object is here:
  # https://github.com/jeffreyhorner/Rook#the-environment
  url_template <- "%s?client_id=%s&redirect_uri=%s&response_type=code&state=%s&access_type=offline&include_granted_scopes=true&scope=%s"
  auth_url <- sprintf(url_template,
    oauth_config$oauth_endpoint_uri,
    utils::URLencode(oauth_config$client_id, reserved = TRUE, repeated = TRUE),
    utils::URLencode(redirect_uri, reserved = TRUE, repeated = TRUE),
    utils::URLencode(state, reserved = TRUE, repeated = TRUE),
    utils::URLencode(oauth_config$scope, reserved = TRUE, repeated = TRUE)
  )

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
    message(jsonlite::toJSON(qs_info, pretty = TRUE, auto_unbox = TRUE))
    return(list(
      status = 500L,
      headers = list("Content-Type" = "text/plain"),
      body = "Authorization failure"
    ))
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
      body = as.character(
        tags$html(
          HTML("<head><script>window.close();</script></head>"),
          tags$body(
            "You can close this window now"
          )
        )
      )
    ))
  } else {
    # TODO: Report malformed request
  }
}

addRouteHandler("/oauth_callback", oauth_callback_handler)


clear_cookie_xsrf <- fastmap::fastmap()

oauth_clear_cookie_handler <- function(req) {
  if (req$REQUEST_METHOD != "POST") {
    return(NULL)
  }

  xsrf_token <- req$rook.input$read_lines(1)
  if (is.null(clear_cookie_xsrf$get(xsrf_token))) {
    return(list(
      status = 403L,
      headers = list(
        "Content-Type" = "text/plain"
      ),
      body = "Unrecognized XSRF token"
    ))
  }
  clear_cookie_xsrf$remove(xsrf_token)

  return(list(
    status = 200L,
    headers = list(
      "Content-Type" = "text/plain",
      "Set-Cookie" = "shinyoauthaccesstoken=; HttpOnly; Path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT"
    ),
    body = ""
  ))
}

addRouteHandler("/oauth_clear_cookie", oauth_clear_cookie_handler)

clear_cookie_custom_handler <- tags$script(
  "
    Shiny.addCustomMessageHandler('oauth-clear-cookie-handler', function(msg) {
      var req = new XMLHttpRequest();
      req.open('POST', 'oauth_clear_cookie');
      req.setRequestHeader('Content-Type', 'text/plain');
      req.send(msg.xsrf_token);
    });
  "
)
