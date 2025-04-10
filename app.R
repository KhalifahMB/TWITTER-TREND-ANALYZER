# app.R - Main Shiny application
# Load required libraries
library(shiny)
library(DT)
library(shinycssloaders) # For loading animations
library(shinyFeedback) # For user feedback
library(shinyjs) # For JavaScript utilities
library(shinybusy) # For loading spinner
library(reticulate) # For Python integration
library(topicmodels) # LDA topic modeling
library(tidytext) # Text mining
library(ggraph) # Network visualization
library(igraph) # Network analysis
library(stringi) # For string manipulation
library(tidyverse)


# Source our modules
source("utils.R")
source("sentiment_analysis.R")
source("twitter_api.R")
source("visualization.R")
source("trends.R")

# Check Python setup at startup
use_virtualenv("./virtualenvs/r-reticulate", required = TRUE)

python_ready <- py_available(initialize = TRUE)

if (!python_ready) {
  stop("Python is not available. Please install Python and required packages.")
}

# UI definition
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyFeedback::useShinyFeedback(),
  titlePanel("📊 Twitter Trend Analyzer with Sentiment Analysis"),
  tags$style(
    HTML("
      .loading-message {
        color: #333;
        font-size: 16px;
        font-weight: bold;
        padding: 10px;
        text-align: center;
      }
      .well {
        background-color: #f9f9f9;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
      }
    ")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(
        class = "well",
        conditionalPanel(
          condition = "input.data_source == 'Fetch from Twitter'",
          textInput(
            "hashtag_input",
            "Enter hashtag (no # needed):",
            value = "dataScience",
            placeholder = "e.g., dataScience"
          ),
          actionButton(
            "fetch_tweets",
            "Fetch Tweets",
            icon = icon("twitter"),
            class = "btn-primary"
          )
        ),
        selectInput(
          "data_source",
          "Select Data Source",
          choices = c("Load from CSV", "Fetch from Twitter")
        ),
        conditionalPanel(
          condition = "input.data_source == 'Load from CSV'",
          fileInput(
            "csv_file",
            "Choose CSV File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        selectInput(
          "sent_method",
          "Select Sentiment Method",
          choices = c("sentimentr", "textblob", "syuzhet")
        ),
        actionButton(
          "analyze",
          "Analyze Tweets",
          icon = icon("chart-bar"),
          class = "btn-success"
        ),
        conditionalPanel(
          condition = "input.data_source == 'Fetch from Twitter'",
          numericInput("hours_back", "Analyze last (hours):", value = 24, min = 1, max = 168)
        ),
        conditionalPanel(
          condition = "input.analyze",
          downloadButton("download_data", "Download Results")
        )
      )
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        type = "tabs",
        # Analysis Tab
        tabPanel(
          "Analysis",
          div(
            class = "loading-message",
            shinycssloaders::withSpinner(
              plotOutput("sentiment_plot", height = "400px"),
              type = 4,
              color = "#0dc5c1"
            )
          ),
          br(),
          div(
            class = "loading-message",
            shinycssloaders::withSpinner(
              plotOutput("word_cloud", height = "400px"),
              type = 4,
              color = "#0dc5c1"
            )
          )
        ),

        # Tweets Tab
        tabPanel(
          "Tweets",
          div(
            class = "loading-message",
            shinycssloaders::withSpinner(
              DTOutput("tweet_table"),
              type = 4,
              color = "#0dc5c1"
            )
          )
        ),

        # Trend Analysis Tab
        tabPanel(
          "Trend Analysis",
          div(
            class = "loading-message",
            shinycssloaders::withSpinner(
              plotOutput("trend_volume_plot", height = "400px"),
              type = 4, color = "#0dc5c1"
            )
          ),
          br(),
          div(
            class = "loading-message",
            shinycssloaders::withSpinner(
              plotOutput("topic_model_plot", height = "400px"),
              type = 4, color = "#0dc5c1"
            )
          )
        ),

        # Network Analysis Tab
        tabPanel(
          "Network Analysis",
          div(
            class = "loading-message",
            shinycssloaders::withSpinner(
              plotOutput("network_plot", height = "600px"),
              type = 4, color = "#0dc5c1"
            )
          )
        ),

        # Top Trends Tab
        tabPanel(
          "Top Trends",
          DTOutput("trend_table")
        ),

        # About Tab
        tabPanel(
          "About",
          includeMarkdown("README.md")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store tweets
  tweets_data <- reactiveVal(NULL)
  trend_metrics <- reactiveVal(NULL)
  lda_model <- reactiveVal(NULL)
  mention_network <- reactiveVal(NULL)

  # Observe data source changes
  observeEvent(input$data_source, {
    if (input$data_source == "Fetch from Twitter") {
      shinyjs::enable("fetch_tweets")
      shinyjs::disable("csv_file")
    } else {
      shinyjs::disable("fetch_tweets")
      shinyjs::enable("csv_file")
    }
  })

  # Handle CSV file upload
  observeEvent(input$csv_file, {
    req(input$csv_file)

    shinyFeedback::feedbackWarning(
      "csv_file",
      is.null(input$csv_file),
      "Please select a CSV file"
    )

    if (is.null(input$csv_file)) {
      return(NULL)
    }

    # Show loading state
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      color = "#0dc5c1",
      text = "Loading CSV file..."
    )

    # Load data
    data <- load_tweets_csv(input$csv_file$datapath)

    # Remove loading state
    shinybusy::remove_modal_spinner()

    if (is.null(data)) {
      shinyFeedback::feedbackDanger(
        "csv_file",
        TRUE,
        "Failed to load CSV file. Please check the file format."
      )
      return(NULL)
    }
    data$cleaned <- vapply(data$text, clean_tweet, character(1))

    tweets_data(data)
  })

  # Handle fetching tweets from Twitter
  observeEvent(input$fetch_tweets, {
    req(input$hashtag_input)

    # Validate hashtag
    if (!validate_hashtag(input$hashtag_input)) {
      shinyFeedback::feedbackDanger(
        "hashtag_input",
        TRUE,
        "Invalid hashtag. Only letters, numbers and underscores are allowed."
      )
      return(NULL)
    }

    shinyFeedback::hideFeedback("hashtag_input")

    # Show loading state
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      color = "#0dc5c1",
      text = "Fetching tweets from Twitter..."
    )

    # Fetch tweets
    data <- fetch_tweets(input$hashtag_input)

    # Remove loading state
    shinybusy::remove_modal_spinner()

    if (is.null(data)) {
      shinyFeedback::feedbackDanger(
        "fetch_tweets",
        TRUE,
        "Failed to fetch tweets. Please try again later."
      )
      return(NULL)
    }

    tweets_data(data)
  })

  # Handle sentiment analysis
  observeEvent(input$analyze, {
    req(tweets_data())

    # Check if TextBlob is available if selected
    if (input$sent_method == "textblob" && !python_ready) {
      shinyFeedback::feedbackDanger(
        "sent_method",
        TRUE,
        "TextBlob requires Python setup. Please install Python and TextBlob."
      )
      return(NULL)
    }

    # Show loading state
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      color = "#0dc5c1",
      text = "Performing sentiment analysis..."
    )

    # Perform analysis with additional validation
    result <- tryCatch(
      {
        data <- tweets_data()

        # Ensure we have the required columns
        if (!"text" %in% names(data)) {
          stop("Input data doesn't contain tweet text")
        }

        perform_sentiment_analysis(data, input$sent_method)
      },
      error = function(e) {
        message("Sentiment analysis error: ", conditionMessage(e))
        NULL
      }
    )

    # Remove loading state
    shinybusy::remove_modal_spinner()

    if (is.null(result)) {
      shinyFeedback::feedbackDanger(
        "analyze",
        TRUE,
        "Sentiment analysis failed. Please try a different method."
      )
      return(NULL)
    }

    tweets_data(result)
  })

  # Handle trend analysis
  observeEvent(tweets_data(), {
    # print("observing trending analysis")
    # print(tweets_data())
    req(tweets_data())

    # Calculate trend metrics
    withProgress(message = "Analyzing trends...", {
      metrics <- tweets_data() %>%
        extract_hashtags() %>%
        calculate_trend_metrics()
      trend_metrics(metrics)
    })

    # Perform topic modeling
    withProgress(message = "Modeling topics...", {
      model <- perform_topic_modeling(tweets_data())
      lda_model(model)
    })

    # Prepare network data
    withProgress(message = "Analyzing network...", {
      network <- prepare_network_data(tweets_data())
      mention_network(network)
    })
  })


  # Outputs
  output$sentiment_plot <- renderPlot({
    req(tweets_data())
    plot_sentiment(tweets_data())
  })

  output$word_cloud <- renderPlot({
    req(tweets_data())
    generate_wordcloud(tweets_data())
  })

  output$trend_volume_plot <- renderPlot({
    req(trend_metrics())
    plot_trend_volume(trend_metrics())
  })

  output$topic_model_plot <- renderPlot({
    req(lda_model())
    plot_topic_model(lda_model())
  })

  output$network_plot <- renderPlot({
    req(mention_network())
    plot_network(mention_network())
  })

  output$trend_table <- renderDT({
    req(trend_metrics())
    trending <- detect_trending_hashtags(trend_metrics())

    datatable(
      trending,
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      rownames = FALSE,
      caption = "Top Trending Hashtags"
    ) %>%
      formatStyle(
        "peak_velocity",
        background = styleColorBar(trending$peak_velocity, "lightblue"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })


  output$tweet_table <- renderDT({
    req(tweets_data())
    req(input$analyze > 0)

    # Safely prepare table data with encoding handling
    table_data <- tryCatch(
      {
        prepare_tweet_table(tweets_data())
      },
      error = function(e) {
        message("Error preparing tweet table: ", conditionMessage(e))
        data.frame(Error = "Could not prepare tweet data due to encoding issues")
      }
    )

    DT::datatable(
      table_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = "20%", targets = 0),
          list(width = "20%", targets = 1),
          list(width = "5%", targets = 3),
          list(width = "10%", targets = 4),
          list(width = "10%", targets = 5)
        )
      ),
      rownames = TRUE,
      filter = "top"
    )
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("twitter-sentiment-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweets_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui, server)
