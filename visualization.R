# visualization.R - Visualization functions

#' Plot sentiment distribution
plot_sentiment <- function(tweets) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required but not installed")
  }

  if (is.null(tweets) || nrow(tweets) == 0 || !"sentiment_label" %in% names(tweets)) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 1, y = 1, label = "No data available") +
      ggplot2::theme_void())
  }

  # Create proper data frame for plotting
  plot_data <- data.frame(
    sentiment = tweets$sentiment_label,
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = sentiment, fill = sentiment)) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(
      values = c("Positive" = "#4CAF50", "Negative" = "#F44336", "Neutral" = "#9E9E9E"),
      name = "Sentiment"
    ) +
    ggplot2::labs(
      title = "Sentiment Distribution",
      x = "Sentiment",
      y = "Number of Tweets"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "none"
    )
}

#' Generate word cloud
generate_wordcloud <- function(tweets) {
  if (!requireNamespace("wordcloud", quietly = TRUE)) {
    stop("wordcloud package is required but not installed")
  }

  if (is.null(tweets) || nrow(tweets) == 0 || !"cleaned" %in% names(tweets)) {
    return(plot.new())
  }

  words <- unlist(strsplit(paste(tweets$cleaned, collapse = " "), " "))
  words <- words[words != "" & !is.na(words)]

  if (length(words) == 0) {
    return(plot.new())
  }

  word_freq <- sort(table(words), decreasing = TRUE)

  if (length(word_freq) == 0) {
    return(plot.new())
  }

  wordcloud::wordcloud(
    names(word_freq),
    freq = word_freq,
    max.words = 100,
    random.order = FALSE,
    colors = RColorBrewer::brewer.pal(8, "Dark2"),
    scale = c(3, 0.5)
  )
}

#' Prepare data for tweet table
prepare_tweet_table <- function(tweets) {
  if (is.null(tweets) || nrow(tweets) == 0) {
    return(data.frame(Message = "No tweets available"))
  }

  # Ensure text is properly encoded before processing
  tweets$text <- stringi::stri_enc_toutf8(tweets$text, validate = TRUE)

  tweets %>%
    dplyr::select(text, sentiment_score, sentiment_label) %>%
    dplyr::mutate(
      sentiment_score = round(sentiment_score, 3),
      text = purrr::map_chr(text, ~ {
        # Safely truncate text
        tryCatch(
          stringr::str_trunc(.x, width = 100),
          error = function(e) {
            # If truncation fails, just take first 100 characters
            substr(.x, 1, min(100, nchar(.x)))
          }
        )
      })
    )
}
