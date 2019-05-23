library(shiny)

options(shiny.port=8100)

# TODO: Figure out how not to require shiny.port to be set in advance
# TODO: Verify that cookies work in Connect/SSP
# TODO: Whole-page protection behind oauth

source("oauth.R")

ui <- fluidPage(
  "Logged in as",
  textOutput("username", inline = TRUE),
  p(
    oauth_login_ui("oauth_login")
  )
)

server <- function(input, output, session) {

  ### GITHUB

  # token <- callModule(oauth_login, id = "oauth_login",
  #   oauth_endpoint_uri = "https://github.com/login/oauth/authorize",
  #   token_endpoint_uri = "https://github.com/login/oauth/access_token",
  #   app_uri = "http://127.0.0.1:8100/",
  #
  #   # Store client_id and client_secret however you want--just hardcoded for this example
  #   client_id = "700d40c400de637d9780",
  #   client_secret = "e6383430779d9df9b253e7d6b1fb53308033873d",
  #
  #   scope = ""
  # )
  #
  # output$username <- renderText({
  #   if (is.null(token())) {
  #     # Not logged in
  #     "(nobody)"
  #   } else {
  #     resp <- httr::GET("https://api.github.com/user",
  #       httr::add_headers("Authorization" = paste("token", token()))
  #     )
  #
  #     httr::content(resp)$login
  #   }
  # })



  ## GOOGLE

  token <- callModule(oauth_login, id = "oauth_login",
    oauth_endpoint_uri = "https://accounts.google.com/o/oauth2/v2/auth",
    token_endpoint_uri = "https://www.googleapis.com/oauth2/v4/token",
    app_uri = "http://127.0.0.1:8100/",

    # Store client_id and client_secret however you want--just hardcoded for this example
    client_id = "350280321053-7bq89pep4da46df2g66ddjnj6e3qrnie.apps.googleusercontent.com",
    client_secret = "8_AHVNXyKyO3tBAZFAy-2y0B",

    scope = "https://www.googleapis.com/auth/drive.metadata.readonly"
  )

  output$username <- renderText({
    if (is.null(token())) {
      # Not logged in
      "(nobody)"
    } else {
      req <- gargle::request_build(method = "GET", path = "oauth2/v3/tokeninfo",
        params = list(access_token=token()),
        base_url = "https://www.googleapis.com")
      resp <- gargle::request_make(req)
      gargle::response_process(resp)$email
    }
  })
}

shinyApp(ui, server, options = list(port = 8100))
