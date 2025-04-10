# visualization.R - Visualization functions

#' Plot sentiment distribution
plot_sentiment <- function(tweets) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required but not installed")
    return(table_data)
    return(table_data)
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
  if (is.null(tweets) || nrow(tweets) == 0 || !"sentiment_label" %in% names(tweets)) {
    return(data.frame(Message = "No tweets available"))
  }

  # Ensure text is properly encoded before processing
  tweets$text <- stringi::stri_enc_toutf8(tweets$text, validate = TRUE)

  table_data <- tweets %>%
    dplyr::select(text, cleaned, sentiment_score, sentiment_label, created_at) %>%
    dplyr::mutate(
      sentiment_score = ifelse(!is.na(sentiment_score), round(sentiment_score, 3), NA),
      text = purrr::map_chr(text, ~ {
        tryCatch(
          stringr::str_trunc(.x, width = 70),
          error = function(e) substr(.x, 1, min(100, nchar(.x)))
        )
      })
    )
}

#' Plot trend volume over time
plot_trend_volume <- function(trend_metrics) {
  # Data prep - aggregate by hour
  heat_data <- trend_metrics %>%
    mutate(hour = hour(date)) %>%
    group_by(hashtag, hour) %>%
    summarise(avg_volume = mean(volume), .groups = "drop")

  ggplot(heat_data, aes(x = hour, y = hashtag, fill = avg_volume)) +
    geom_tile(color = "white") +
    # scale_fill_gradient(low = "white", high = "steelblue") +
    labs(
      title = "Hourly Hashtag Activity",
      x = "Hour of Day",
      y = "Hashtag",
      fill = "Avg Volume"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0),
      panel.grid = element_blank()
    )
}

#' Plot topic modeling results
plot_topic_model <- function(lda_model) {
  topics <- tidy(lda_model, matrix = "beta")

  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  ggplot(top_terms, aes(reorder(term, beta), beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free") +
    coord_flip() +
    labs(
      title = "Top Terms in Each Topic",
      x = "Term",
      y = "Beta (Term Importance)"
    ) +
    theme_minimal()
}

#' Plot network graph
plot_network <- function(network) {
  ggraph(network, layout = "fr") +
    geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) +
    geom_node_point(size = 3, color = "steelblue") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    labs(
      title = "User Mention Network",
      subtitle = "Node size represents influence"
    ) +
    theme_void()
}
