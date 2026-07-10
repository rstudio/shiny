# Builds the `ui://shiny/app` MCP Apps resource: the app's rendered UI page
# as a single self-contained HTML document. MCP hosts render this HTML in a
# sandboxed iframe with a deny-by-default Content Security Policy (and some
# hosts currently ignore declared CSP domains entirely), so every script and
# stylesheet must be inlined and the MCP bridge script (which replaces
# Shiny's websocket with the postMessage tunnel) is injected at the end of
# <body>.

# Is this session connected through the MCP Apps tunnel? (Marker set on the
# fake websocket request by McpConnection; see mcp-tunnel.R.)
isMcpSession <- function(session) {
  req <- tryCatch(suppressWarnings(session$request), error = function(e) NULL)
  if (is.null(req)) {
    return(FALSE)
  }
  identical(
    tryCatch(req$HTTP_MCP_TUNNEL, error = function(e) NULL),
    "1"
  )
}

# Convert a file-based htmlDependency into one whose scripts/stylesheets are
# inline <script>/<style> tags in `head`. Used for dynamic UI in MCP
# sessions: the sandboxed iframe cannot fetch `src`/`href` URLs, but the
# client executes `head` content (appendExtraHeadContent in
# srcts/src/shiny/render.ts uses jQuery, which runs inline scripts).
mcpInlineDependency <- function(dependency) {
  if (is.null(dependency)) {
    return(NULL)
  }
  root <- dependency$src$file
  if (is.null(root)) {
    # Nothing to inline (href-only); serve as usual.
    return(createWebDependency(dependency))
  }
  if (!is.null(dependency$package)) {
    root <- system_file(root, package = dependency$package)
  }

  script_files <- vapply(
    dependency$script %||% character(0),
    function(s) if (is.list(s)) s$src %||% "" else s,
    character(1)
  )
  parts <- character(0)
  for (f in script_files) {
    path <- file.path(root, f)
    if (nzchar(f) && file.exists(path)) {
      parts <- c(parts, paste0(
        "<script>\n", escapeScriptContent(readAssetText(path)), "\n</script>"
      ))
    }
  }
  for (f in dependency$stylesheet %||% character(0)) {
    path <- file.path(root, f)
    if (file.exists(path)) {
      parts <- c(parts, inlineStylesheetFile(path))
    }
  }
  if (!is.null(dependency$head)) {
    parts <- c(parts, dependency$head)
  }

  htmltools::htmlDependency(
    name = dependency$name,
    version = dependency$version,
    src = c(href = "_mcp_inline"),
    head = paste(parts, collapse = "\n"),
    all_files = FALSE
  )
}

mcpFakePageRequest <- function() {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- "GET"
  req$PATH_INFO <- "/"
  req$QUERY_STRING <- ""
  req$HTTP_HOST <- "127.0.0.1"
  req
}

renderMcpAppHtml <- function(uiHandler) {
  resp <- uiHandler(mcpFakePageRequest())
  if (is.null(resp) || !inherits(resp, "httpResponse")) {
    stop("Unable to render the app UI for the MCP app resource")
  }
  html <- resp$content
  if (is.raw(html)) {
    html <- rawToChar(html)
    Encoding(html) <- "UTF-8"
  }
  html <- inlineHtmlAssets(html)
  sub("</body>", paste0(mcpBridgeScript(), "\n</body>"), html, fixed = TRUE)
}

mcpBridgeScript <- function() {
  path <- system_file("www/shared/shiny-mcp-bridge.js", package = "shiny")
  js <- readAssetText(path)
  paste0(
    "<script data-shiny-mcp-bridge>\n",
    "// shiny-mcp-bridge\n",
    escapeScriptContent(js),
    "\n</script>"
  )
}

readAssetText <- function(file) {
  txt <- readChar(file, file.info(file)$size, useBytes = TRUE)
  Encoding(txt) <- "UTF-8"
  txt
}

# Rewrite relative `url(...)` references in a stylesheet to data: URIs,
# resolving them against the stylesheet's directory on disk. Once a
# stylesheet is inlined into the single-file MCP app resource, its relative
# URLs (fonts, background images — e.g. fontawesome's ../webfonts/*.woff2)
# would resolve against the sandbox origin and 404. The referenced files are
# part of the app's own dependencies, so embedding them is safe.
#
# Left untouched: data:/blob: URIs, absolute and protocol-relative URLs,
# fragment-only refs (SVG paint servers like url(#gradient)), and paths that
# don't resolve to an existing file.
rewriteCssUrls <- function(css, cssDir) {
  # perl = TRUE: TRE fails on backreferences to groups that matched the
  # empty string (unquoted url(...) refs); PCRE handles them.
  pattern <- "url\\(\\s*(['\"]?)([^'\")]+)\\1\\s*\\)"
  matches <- gregexpr(pattern, css, ignore.case = TRUE, perl = TRUE)
  cache <- new.env(parent = emptyenv())

  regmatches(css, matches) <- list(vapply(
    regmatches(css, matches)[[1]],
    function(token) {
      ref <- trimws(sub(pattern, "\\2", token, ignore.case = TRUE, perl = TRUE))
      if (grepl("^(data:|blob:|https?://|//|#)", ref, ignore.case = TRUE)) {
        return(token)
      }
      path <- sub("[?#].*$", "", ref)
      if (!nzchar(path)) {
        return(token)
      }
      # Legacy font fallbacks: @font-face src lists are tried in order, so
      # when the (inlined) woff2 loads, ttf/otf/eot are never fetched.
      # Inlining them would double-to-triple the resource size for nothing.
      if (grepl("\\.(ttf|otf|eot)$", path, ignore.case = TRUE)) {
        return(token)
      }
      candidate <- file.path(cssDir, URLdecode(path))
      if (!file.exists(candidate)) {
        return(token)
      }
      candidate <- normalizePath(candidate)
      cached <- cache[[candidate]]
      if (!is.null(cached)) {
        return(cached)
      }
      bytes <- readBin(candidate, "raw", n = file.info(candidate)$size)
      # base64_enc() line-wraps long output; a raw newline inside a quoted
      # CSS string is invalid and (in minified, single-line stylesheets)
      # can swallow the rest of the sheet.
      b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(bytes))
      out <- sprintf(
        'url("data:%s;base64,%s")',
        getContentType(candidate),
        b64
      )
      cache[[candidate]] <- out
      out
    },
    character(1),
    USE.NAMES = FALSE
  ))
  css
}

inlineStylesheetFile <- function(file) {
  css <- rewriteCssUrls(readAssetText(file), dirname(file))
  paste0("<style>\n", escapeStyleContent(css), "\n</style>")
}

# `</script` anywhere inside an inline <script> terminates it, even inside a
# JS string. `<\/script` is equivalent inside JS strings/regexes, where such
# sequences virtually always live.
escapeScriptContent <- function(x) {
  gsub("</(script)", "<\\\\/\\1", x, ignore.case = TRUE)
}

escapeStyleContent <- function(x) {
  gsub("</(style)", "<\\\\/\\1", x, ignore.case = TRUE)
}

# Map a URL path from the rendered page to a local file, using the resource
# paths registered by createWebDependency()/addResourcePath() plus shiny's
# shared www dir. Returns NULL if the URL cannot be resolved locally.
resolveAssetFile <- function(urlPath) {
  urlPath <- sub("^\\./", "", sub("^/", "", URLdecode(urlPath)))
  urlPath <- sub("[?#].*$", "", urlPath)
  parts <- strsplit(urlPath, "/", fixed = TRUE)[[1]]
  if (length(parts) < 2) {
    return(NULL)
  }
  prefix <- parts[1]
  rest <- paste(parts[-1], collapse = "/")

  target <- .globals$resourcePaths[[prefix]]
  root <- if (!is.null(target)) {
    if (inherits(target, "staticPath")) target$path else as.character(target)
  } else if (identical(prefix, "shared")) {
    system_file("www/shared", package = "shiny")
  } else {
    return(NULL)
  }

  candidate <- file.path(root, rest)
  if (file.exists(candidate)) candidate else NULL
}

inlineHtmlAssets <- function(html) {
  # <script src="..."></script>
  html <- inlineTagPattern(
    html,
    pattern = "<script[^>]*\\ssrc=\"([^\"]+)\"[^>]*>\\s*</script>",
    build = function(file) {
      paste0("<script>\n", escapeScriptContent(readAssetText(file)), "\n</script>")
    }
  )
  # <link rel="stylesheet" href="..."/>
  inlineTagPattern(
    html,
    pattern = "<link[^>]*\\shref=\"([^\"]+)\"[^>]*/?>",
    build = inlineStylesheetFile,
    onlyIf = function(tag) grepl("stylesheet", tag, fixed = TRUE)
  )
}

inlineTagPattern <- function(html, pattern, build, onlyIf = NULL) {
  matches <- gregexpr(pattern, html, ignore.case = TRUE)[[1]]
  if (identical(as.integer(matches[1]), -1L)) {
    return(html)
  }

  lengths <- attr(matches, "match.length")
  out <- character(0)
  last <- 1
  for (i in seq_along(matches)) {
    start <- matches[i]
    end <- start + lengths[i] - 1
    tag <- substr(html, start, end)
    replacement <- tag
    if (is.null(onlyIf) || isTRUE(onlyIf(tag))) {
      url <- sub(pattern, "\\1", tag, ignore.case = TRUE)
      file <- resolveAssetFile(url)
      if (!is.null(file)) {
        replacement <- build(file)
      } else {
        # The sandbox can't load external refs anyway; drop them so the
        # document doesn't point at unreachable URLs.
        replacement <- ""
      }
    }
    out <- c(out, substr(html, last, start - 1), replacement)
    last <- end + 1
  }
  out <- c(out, substr(html, last, nchar(html)))
  paste(out, collapse = "")
}
