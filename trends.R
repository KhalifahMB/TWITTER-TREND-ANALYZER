# trends.R - Robust Trend Analysis Functions

#' Safely Extract Hashtags from Tweets
#'
#' @param tweets A dataframe containing tweet data
#' @param text_column Name of column containing tweet text (default: "text")
#' @return Dataframe with additional "hashtags" column
#' @export
extract_hashtags <- function(tweets, text_column = "text") {
  # Validate input
  if (!is.data.frame(tweets)) {
    stop("Input must be a dataframe")
  }

  if (!text_column %in% names(tweets)) {
    stop("Text column '", text_column, "' not found in input data")
  }

  # QUESTION: Should we keep tweets with no hashtags (keep_empty=TRUE)?
  # Currently preserving them but this affects downstream analysis
  tryCatch(
    {
      tweets %>%
        mutate(
          hashtags = str_extract_all(.data[[text_column]], "#\\w+"),
          hashtags = map(hashtags, ~ str_remove_all(.x, "#")),
          hashtags = map(hashtags, tolower)
        ) %>%
        unnest(hashtags, keep_empty = TRUE)
    },
    error = function(e) {
      stop("Hashtag extraction failed: ", e$message)
    }
  )
}

#' Calculate Comprehensive Trend Metrics
#'
#' @param tweets Dataframe containing tweets with:
#'        - created_at: Timestamp column
#'        - hashtags: Extracted hashtags column
#' @param time_unit Time bucket size (default: "1 hour")
#' @param spike_threshold Quantile for spike detection (default: 0.95)
#' @return Dataframe with trend metrics
#' @export
calculate_trend_metrics <- function(tweets,
                                    time_unit = "1 hour",
                                    spike_threshold = 0.95) {
  # Validate input structure
  required_cols <- c("created_at", "hashtags")
  missing_cols <- setdiff(required_cols, names(tweets))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # QUESTION: How should we handle invalid timestamps? Currently filtering them
  tryCatch(
    {
      tweets %>%
        # Parse and validate timestamps
        mutate(
          created_at = lubridate::ymd_hms(created_at),
          is_valid_time = !is.na(created_at)
        ) %>%
        filter(is_valid_time) %>%
        select(-is_valid_time) %>%
        # Handle empty/missing hashtags
        mutate(
          hashtag = case_when(
            is.na(hashtags) ~ "no_hashtag",
            hashtags == "" ~ "no_hashtag",
            TRUE ~ hashtags
          )
        ) %>%
        # Group and summarize
        group_by(hashtag, date = floor_date(created_at, time_unit)) %>%
        summarise(
          volume = n(),
          .groups = "drop"
        ) %>%
        # Calculate velocity and detect spikes
        arrange(date) %>%
        group_by(hashtag) %>%
        mutate(
          velocity = volume - lag(volume, default = first(volume)),
          spike = case_when(
            is.na(velocity) ~ "Insufficient data",
            velocity > quantile(velocity, spike_threshold, na.rm = TRUE) ~ "Hot",
            TRUE ~ "Normal"
          )
        ) %>%
        ungroup()
    },
    error = function(e) {
      stop("Trend metric calculation failed: ", e$message)
    }
  )
}

#' Identify Top Trending Hashtags
#'
#' @param trend_metrics Dataframe from calculate_trend_metrics()
#' @param n_top Number of top trends to return (default: 10)
#' @return Dataframe of top trending hashtags
#' @export
detect_trending_hashtags <- function(trend_metrics, n_top = 10) {
  # Validate input structure
  required_cols <- c("hashtag", "volume", "velocity")
  missing_cols <- setdiff(required_cols, names(trend_metrics))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  tryCatch(
    {
      trend_metrics %>%
        group_by(hashtag) %>%
        summarise(
          total_volume = sum(volume, na.rm = TRUE),
          peak_velocity = max(velocity, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(peak_velocity)) %>%
        head(n_top)
    },
    error = function(e) {
      stop("Trend detection failed: ", e$message)
    }
  )
}

#' Perform Robust Topic Modeling
#'
#' @param tweets Dataframe containing:
#'        - id: Unique tweet identifier
#'        - cleaned: Preprocessed text content
#' @param num_topics Number of topics to model (default: 5)
#' @return LDA model object
#' @export
perform_topic_modeling <- function(tweets, num_topics = 5) {
  # Validate input
  required_cols <- c("id", "cleaned")
  missing_cols <- setdiff(required_cols, names(tweets))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  tryCatch(
    {
      # QUESTION: Should we add text cleaning verification here?
      dtm <- tweets %>%
        unnest_tokens(word, cleaned) %>%
        anti_join(stop_words, by = "word") %>%
        count(id, word) %>%
        cast_dtm(id, word, n)

      LDA(dtm, k = num_topics, control = list(seed = 1234))
    },
    error = function(e) {
      stop("Topic modeling failed: ", e$message)
    }
  )
}

#' Prepare Network Graph Data with Validation
#'
#' @param tweets Dataframe containing:
#'        - text: Raw tweet text
#'        - author_id: Unique author identifier
#' @return igraph network object
#' @export
prepare_network_data <- function(tweets) {
  # Validate input
  required_cols <- c("text", "author_id")
  missing_cols <- setdiff(required_cols, names(tweets))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  tryCatch(
    {
      mentions <- tweets %>%
        mutate(
          mentions = str_extract_all(text, "@\\w+"),
          mentions = map(mentions, ~ .x[!is.na(.x) & .x != ""]) # Filter invalid
        ) %>%
        unnest(mentions) %>%
        select(from = author_id, to = mentions)

      # QUESTION: Should we filter self-mentions (from == to)?
      graph_from_data_frame(mentions, directed = TRUE)
    },
    error = function(e) {
      stop("Network preparation failed: ", e$message)
    }
  )
}
