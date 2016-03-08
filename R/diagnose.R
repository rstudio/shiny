# Analyze an R file for possible extra or missing commas. Returns FALSE if any
# problems detected, TRUE otherwise.
diagnoseCode <- function(path = NULL, text = NULL) {
  if (!xor(is.null(path), is.null(text))) {
    stop("Must specify `path` or `text`, but not both.")
  }

  if (!is.null(path)) {
    tokens <- sourcetools::tokenize_file(path)
  } else {
    tokens <- sourcetools::tokenize_string(text)
  }

  find_scopes <- function(tokens) {
    # Strip whitespace and comments
    tokens <- tokens[!(tokens$type %in% c("whitespace", "comment")),]

    # Replace various types of things with "value"
    tokens$type[tokens$type %in% c("string", "number", "symbol", "keyword")] <- "value"

    # Record types for close and open brace/bracket/parens, and commas
    brace_idx <- tokens$value %in% c("(", ")", "{", "}", "[", "]", ",")
    tokens$type[brace_idx] <- tokens$value[brace_idx]

    # Stack-related function for recording scope. Starting scope is "{"
    stack <- "{"
    push <- function(x) {
      stack <<- c(stack, x)
    }
    pop <- function() {
      if (length(stack) == 1) {
        # Stack underflow, but we need to keep going
        return(NA_character_)
      }
      res <- stack[length(stack)]
      stack <<- stack[-length(stack)]
      res
    }
    peek <- function() {
      stack[length(stack)]
    }

    # First, establish a scope for each token. For opening and closing
    # braces/brackets/parens, the scope at that location is the *surrounding*
    # scope, not the new scope created by the brace/bracket/paren.
    for (i in seq_len(nrow(tokens))) {
      value <- tokens$value[i]

      tokens$scope[i] <- peek()
      if (value %in% c("{", "(", "[")) {
        push(value)

      } else if (value == "}") {
        if (!identical(pop(), "{"))
          tokens$err[i] <- "unmatched_brace"
        # For closing brace/paren/bracket, get the scope after popping
        tokens$scope[i] <- peek()

      } else if (value == ")") {
        if (!identical(pop(), "("))
          tokens$err[i] <- "unmatched_paren"
        tokens$scope[i] <- peek()

      } else if (value == "]") {
        if (!identical(pop(), "["))
          tokens$err[i] <- "unmatched_bracket"
        tokens$scope[i] <- peek()
      }
    }

    tokens
  }

  check_commas <- function(tokens) {
    # Find extra and missing commas
    tokens$err <- mapply(
      tokens$type,
      c("", tokens$type[-length(tokens$type)]),
      c(tokens$type[-1],  ""),
      tokens$scope,
      tokens$err,
      SIMPLIFY = FALSE,
      FUN = function(type, prevType, nextType, scope, err) {
        # If an error was already found, just return it. This could have
        # happened in the brace/paren/bracket matching phase.
        if (!is.na(err)) {
          return(err)
        }
        if (scope == "(") {
          if (type == "," &&
              (prevType == "(" || prevType == "," || nextType == ")"))
          {
            return("extra_comma")
          }

          if ((prevType == ")" && type == "value") ||
              (prevType == "value" && type == "value")) {
            return("missing_comma")
          }
        }

        NA_character_
      }
    )

    tokens
  }


  tokens$err <- NA_character_
  tokens <- find_scopes(tokens)
  tokens <- check_commas(tokens)

  # No errors found
  if (all(is.na(tokens$err))) {
    return(TRUE)
  }

  # If we got here, errors were found; print messages.
  if (!is.null(path)) {
    lines <- readLines(path)
  } else {
    lines <- strsplit(text, "\n")[[1]]
  }

  # Print out the line of code with the error, and point to the column with
  # the error.
  show_code_error <- function(msg, lines, row, col) {
    message(paste0(
      msg, "\n",
      row, ":", lines[row], "\n",
      paste0(rep.int(" ", nchar(as.character(row)) + 1), collapse = ""),
      gsub(perl = TRUE, "[^\\s]", " ", substr(lines[row], 1, col-1)), "^"
    ))
  }

  err_idx <- which(!is.na(tokens$err))
  msg <- ""
  for (i in err_idx) {
    row <- tokens$row[i]
    col <- tokens$column[i]
    err <- tokens$err[i]

    if (err == "missing_comma") {
      show_code_error("Possible missing comma at:", lines, row, col)
    } else if (err == "extra_comma") {
      show_code_error("Possible extra comma at:", lines, row, col)
    } else if (err == "unmatched_brace") {
      show_code_error("Possible unmatched '}' at:", lines, row, col)
    } else if (err == "unmatched_paren") {
      show_code_error("Possible unmatched ')' at:", lines, row, col)
    } else if (err == "unmatched_bracket") {
      show_code_error("Possible unmatched ']' at:", lines, row, col)
    }
  }
  return(FALSE)
}
