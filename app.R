library(shiny)
library(shinyjs)

# UI definition
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  titlePanel("Cambist Booking"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      actionButton("reset", "Reset"),
      actionButton("nextBtn", "Next"),
      actionButton("closeBook", "Close Book"),
      # Simplified debug output
      textOutput("debug")
    ),
    
    mainPanel(
      verbatimTextOutput("text1"),
      verbatimTextOutput("text2"),
      textOutput("position")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the text strings
  text_data <- reactiveVal(NULL)
  # Reactive value to store current position
  current_pos <- reactiveVal(1)
  # Reactive value to track if we're in "closed book" mode
  closed_book <- reactiveVal(FALSE)
  
  # Handle file upload
  observeEvent(input$file, {
    # Read the single line and split it by commas
    text_content <- readLines(input$file$datapath)
    items <- unlist(strsplit(text_content, ","))
    
    # Clean up the items (remove whitespace)
    items <- trimws(items)
    
    # Remove any empty items
    items <- items[nzchar(items)]
    
    # Store if we have data, and randomize immediately
    if(length(items) > 0) {
      text_data(sample(items))  # Randomize on load
      current_pos(1)
      closed_book(FALSE)
      enable("nextBtn")
    }
  })
  
  # Debug output - just number of items
  output$debug <- renderText({
    if (!is.null(text_data())) {
      paste("Number of items loaded:", length(text_data()))
    } else {
      "No data loaded"
    }
  })
  
  # Handle reset button
  observeEvent(input$reset, {
    if (!is.null(text_data()) && length(text_data()) > 0) {
      # Randomize the order
      text_data(sample(text_data()))
      # Reset position
      current_pos(1)
      closed_book(FALSE)
      enable("nextBtn")
    }
  })
  
  # Handle next button
  observeEvent(input$nextBtn, {
    if (!is.null(text_data()) && length(text_data()) > 1 && !closed_book()) {
      # Only advance if we're not at the end
      if (current_pos() < length(text_data()) - 1) {
        current_pos(current_pos() + 1)
      }
    }
  })
  
  # Handle close book button
  observeEvent(input$closeBook, {
    if (!is.null(text_data()) && length(text_data()) > 1) {
      # Only advance if we're not at the last item
      if (current_pos() < length(text_data())) {
        current_pos(current_pos() + 1)
      }
      closed_book(TRUE)
      disable("nextBtn")
    }
  })
  
  # Output for first text string
  output$text1 <- renderText({
    if (!is.null(text_data()) && length(text_data()) > 0) {
      text_data()[current_pos()]
    } else {
      "No data loaded"
    }
  })
  
  # Output for second text string
  output$text2 <- renderText({
    if (!is.null(text_data()) && length(text_data()) > 0) {
      if (closed_book()) {
        # If in closed book mode, show first item
        text_data()[1]
      } else if (current_pos() < length(text_data())) {
        # Normal mode, show next item
        text_data()[current_pos() + 1]
      } else {
        "No more data"
      }
    } else {
      "No data loaded"
    }
  })
  
  # Output for position indicator
  output$position <- renderText({
    if (!is.null(text_data()) && length(text_data()) > 0) {
      if (closed_book()) {
        paste("Position:", current_pos(), "& 1 (Closed Book)")
      } else {
        paste("Position:", current_pos(), "of", length(text_data()) - 1)
      }
    } else {
      "No data loaded"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)