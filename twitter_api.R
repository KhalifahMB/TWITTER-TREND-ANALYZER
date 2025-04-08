# twitter_api.R - Twitter API functions

#' Initialize Twitter API client
initialize_twitter_client <- function() {
  bearer_token <- Sys.getenv("TWITTER_BEARER_TOKEN")

  if (is.null(bearer_token) || bearer_token == "") {
    stop("Twitter Bearer Token not found in environment variables")
  }

  httr::add_headers(Authorization = paste("Bearer", bearer_token))
}

#' Fetch tweets from Twitter API
fetch_tweets <- function(query, max_results = 100) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package is required but not installed")
  }

  headers <- initialize_twitter_client()

  # API parameters
  params <- list(
    query = paste("#", query, " lang:en", sep = ""),
    max_results = max_results,
    tweet.fields = "created_at,public_metrics",
    user.fields = "username",
    expansions = "author_id"
  )

  response <- safe_operation(
    {
      httr::GET(
        url = "https://api.twitter.com/2/tweets/search/recent",
        headers,
        query = params
      )
    },
    error_value = NULL
  )

  if (is.null(response)) {
    return(NULL)
  }

  # Check rate limits
  rate_limit <- httr::headers(response)$`x-rate-limit-remaining`
  if (!is.null(rate_limit) && as.numeric(rate_limit) < 10) {
    warning(sprintf("Low rate limit remaining: %s", rate_limit))
  }

  # Process response
  if (httr::http_status(response)$category == "Success") {
    content_raw <- httr::content(response, as = "text", encoding = "UTF-8")
    content_list <- jsonlite::fromJSON(content_raw, flatten = TRUE)

    if (!is.null(content_list$data)) {
      tweets <- as.data.frame(content_list$data, stringsAsFactors = FALSE)
      return(tweets)
    }
  }

  return(NULL)
}
