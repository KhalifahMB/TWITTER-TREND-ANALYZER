# Twitter Trend Analyzer with Sentiment Analysis

## About This Application

This Shiny application analyzes Twitter trends and performs sentiment analysis on tweets using three different methods:

1. **sentimentr** - R package for text sentiment analysis
2. **TextBlob** - Python library for processing textual data
3. **syuzhet** - R package implementing multiple sentiment dictionaries

## Features

- Fetch tweets from Twitter API using hashtags
- Upload and analyze tweets from CSV files
- Three different sentiment analysis methods
- Interactive visualizations including:
  - Sentiment distribution bar chart
  - Word cloud of most frequent terms
- Data table with filtering capabilities
- Download results as CSV

## Requirements

- R packages: shiny, DT, sentimentr, syuzhet, reticulate, wordcloud, tidytext, ggplot2, dplyr, stringr
- For TextBlob analysis: Python with TextBlob installed
- For Twitter API: Valid Twitter Bearer Token in environment variables

## Usage Instructions

1. Select data source (CSV or Twitter API)
2. If using Twitter API, enter a hashtag (without #)
3. Select sentiment analysis method
4. Click "Analyze Tweets" button
5. View results in the Analysis and Tweets tabs
6. Download results if needed

## Troubleshooting

If TextBlob analysis fails:
- Ensure Python is installed
- Run `reticulate::py_install("textblob")` in R console

If Twitter API fails:
- Check your bearer token is set in environment variables
- Verify you have API access and sufficient rate limits