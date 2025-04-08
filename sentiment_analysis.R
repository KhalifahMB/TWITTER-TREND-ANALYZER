# sentiment_analysis.R - Sentiment analysis functions

#' Calculate sentiment using sentimentr
calculate_sentimentr <- function(tweets) {
  if (!requireNamespace("sentimentr", quietly = TRUE)) {
    stop("sentimentr package is required but not installed")
  }

  tweets$cleaned <- vapply(tweets$text, clean_tweet, character(1))
  sent_result <- sentimentr::sentiment_by(tweets$cleaned)

  tweets$sentiment_score <- sent_result$ave_sentiment
  tweets$sentiment_label <- cut(
    tweets$sentiment_score,
    breaks = c(-Inf, -0.1, 0.1, Inf),
    labels = c("Negative", "Neutral", "Positive"),
    right = FALSE
  )

  return(tweets)
}

#' Calculate sentiment using TextBlob via reticulate
calculate_textblob <- function(tweets) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate package is required but not installed")
  }

  if (!reticulate::py_module_available("textblob")) {
    stop("Python TextBlob module is not available")
  }

  textblob <- reticulate::import("textblob")

  tweets$cleaned <- vapply(tweets$text, clean_tweet, character(1))

  # Safe calculation with error handling
  tweets$sentiment_score <- vapply(
    tweets$cleaned,
    function(t) {
      safe_operation(
        textblob$TextBlob(t)$sentiment$polarity,
        error_value = NA_real_
      )
    },
    numeric(1)
  )

  tweets$sentiment_label <- cut(
    tweets$sentiment_score,
    breaks = c(-Inf, -0.1, 0.1, Inf),
    labels = c("Negative", "Neutral", "Positive"),
    right = FALSE
  )

  return(tweets)
}

#' Calculate sentiment using syuzhet
calculate_syuzhet <- function(tweets) {
  if (!requireNamespace("syuzhet", quietly = TRUE)) {
    stop("syuzhet package is required but not installed")
  }

  tweets$cleaned <- vapply(tweets$text, clean_tweet, character(1))
  tweets$sentiment_score <- syuzhet::get_sentiment(
    tweets$cleaned,
    method = "syuzhet"
  )

  tweets$sentiment_label <- cut(
    tweets$sentiment_score,
    breaks = c(-Inf, -0.1, 0.1, Inf),
    labels = c("Negative", "Neutral", "Positive"),
    right = FALSE
  )

  return(tweets)
}

#' Dispatch to appropriate sentiment function
perform_sentiment_analysis <- function(tweets, method) {
  if (is.null(tweets) || nrow(tweets) == 0 || !"text" %in% names(tweets)) {
    stop("No valid tweet data provided for sentiment analysis")
  }

  result <- switch(method,
    "sentimentr" = calculate_sentimentr(tweets),
    "textblob" = calculate_textblob(tweets),
    "syuzhet" = calculate_syuzhet(tweets),
    stop("Unknown sentiment analysis method: ", method)
  )

  # Ensure we have the required columns
  required_cols <- c("text", "cleaned", "sentiment_score", "sentiment_label")
  missing_cols <- setdiff(required_cols, names(result))

  if (length(missing_cols) > 0) {
    stop(
      "Sentiment analysis failed to create required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  return(result)
}
