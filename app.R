# app.R - Main Shiny application

# Load required libraries
library(shiny)
library(DT)
library(shinycssloaders) # For loading animations
library(shinyFeedback) # For user feedback
library(shinyjs) # For JavaScript utilities
library(shinybusy) # For loading spinner
library(reticulate) # For Python integration
library(stringi) # For string manipulation

# Source our modules
source("utils.R")
source("sentiment_analysis.R")
source("twitter_api.R")
source("visualization.R")

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
      width = 3,
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
          condition = "input.analyze > 0",
          downloadButton("download_data", "Download Results")
        )
      )
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        type = "tabs",
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
        tabPanel(
          "About",
          includeMarkdown("about.md")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store tweets
  tweets_data <- reactiveVal(NULL)

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

  # Outputs
  output$sentiment_plot <- renderPlot({
    req(tweets_data())
    plot_sentiment(tweets_data())
  })

  output$word_cloud <- renderPlot({
    req(tweets_data())
    generate_wordcloud(tweets_data())
  })

  output$tweet_table <- renderDT({
    req(tweets_data())

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
          list(width = "60%", targets = 0),
          list(width = "20%", targets = 1),
          list(width = "20%", targets = 2)
        )
      ),
      rownames = FALSE,
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
