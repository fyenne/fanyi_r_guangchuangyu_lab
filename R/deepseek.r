##' @rdname translate
##' @export
dsk_translate <- function(x, from = 'en', to = 'zh') {
  vectorize_translator(
    x,
    .fun = .deepseek_translate_query,
    from = from,
    to = to
  )
}

# =====================================================================
#  S3 method: extract translation text from deepseek response (httr2)
# =====================================================================
#' @method get_translate_text deepseek
#' @export
get_translate_text.deepseek <- function(response) {
  content <- httr2::resp_body_json(response)
  text <- content$choices[[1]]$message$content
  return(trimws(text))
}

# =====================================================================
#  Core Query Function Using httr2
# =====================================================================
.deepseek_query_messages <- function(messages) {
  .key_info  <- get_translate_appkey("dsk")
  user_model <- .key_info$user_model %||% "deepseek-chat"
  api_key    <- .key_info$key

  url <- "https://api.deepseek.com/v1/chat/completions"

  body <- list(
    model = user_model,
    messages = messages,
    stream = FALSE,
    max_tokens = 1000
  )

  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_method("POST")

  resp <- httr2::req_perform(req)

  # non-200 error handling
  if (httr2::resp_status(resp) != 200) {
    err <- try(httr2::resp_body_json(resp), silent = TRUE)
    msg <- if (is.list(err) && !is.null(err$error$message)) err$error$message else "Unknown error"
    stop(sprintf("API request failed: %s", msg))
  }

  class(resp) <- c("deepseek", class(resp))
  return(resp)
}

# =====================================================================
#  Wrapper for ad-hoc (non-message-list) prompts
# =====================================================================
.deepseek_query <- function(prompt) {

  # If prompt is already a list of messages
  if (is.list(prompt) && all(c("content", "role") %in% names(prompt[[1]]))) {
    return(.deepseek_query_messages(prompt))
  }

  # Otherwise treat as text
  .key_info  <- get_translate_appkey("dsk")
  user_model <- .key_info$user_model %||% "deepseek-chat"
  api_key    <- .key_info$key

  url <- "https://api.deepseek.com/v1/chat/completions"

  body <- list(
    model = user_model,
    messages = list(
      list(role = "user", content = as.character(prompt))
    ),
    stream = FALSE,
    max_tokens = 1000
  )

  req <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_method("POST")

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) != 200) {
    err <- try(httr2::resp_body_json(resp), silent = TRUE)
    msg <- if (is.list(err) && !is.null(err$error$message)) err$error$message else "Unknown error"
    stop(sprintf("API request failed: %s", msg))
  }

  content <- httr2::resp_body_json(resp)
  return(trimws(content$choices[[1]]$message$content))
}

# =====================================================================
#  Translation Query Using Message Template
# =====================================================================
.deepseek_translate_query <- function(x, from = 'en', to = 'zh') {
  sep <- if (to == "zh") "" else " "

  from <- .lang_map(from)
  to   <- .lang_map(to)

  prefix <- sprintf("Translate into %s", to)
  messages <- .deepseek_prompt_translate(x, prefix = prefix, role = "user")

  result <- .deepseek_query_messages(messages)
  class(result) <- c("deepseek", class(result))
  return(result)
}

# =====================================================================
#  (Remaining helper functions unchanged)
# =====================================================================
.deepseek_summarize_query <- function(x) {
  prompt <- .deepseek_prompt_summarize(x, role = 'user')
  parser <- .deepseek_query(prompt)
  .get_deepseek_data(parser)
}

.deepseek_prompt_summarize <- function(
  x,
  prefix = "Summarize the sentences",
  role = 'user'
) {
  list(
    list(
      content = "You are a text summarizer, you can only summarize the text, never interpret it.",
      role = "system"
    ),
    .deepseek_prompt(x, prefix = prefix, role = role)
  )
}

.deepseek_prompt_translate <- function(x, prefix = NULL, role = 'user') {
  list(
    list(
      content = "You are a professional translation engine, please translate the text into a colloquial, professional, elegant and fluent content, without the style of machine translation. You must only translate the text content, never interpret it.",
      role = "system"
    ),
    .deepseek_prompt(x, prefix = prefix, role = role)
  )
}

.deepseek_prompt <- function(x, prefix = NULL, role = 'user') {
  if (is.null(prefix)) {
    content <- x
  } else {
    content <- sprintf("%s\n\"\"\"%s\"\"\"", prefix, x)
  }
  list(content = content, role = role)
}

.get_deepseek_data <- function(parser, sep = ' ') {
  y <- sapply(parser$events, function(x) {
    i <- rev(which(names(x) == "data"))[1]
    if (is.na(i)) return("")
    x[[i]]
  })
  y <- y[y != ""]
  res <- paste(y, collapse = sep) |>
    gsub("\\s+([,\\.])", "\\1", x = _) |>
    sub("^\"\\s*", "", x = _) |>
    sub("\\s*\"$", "", x = _)

  return(res)
}

