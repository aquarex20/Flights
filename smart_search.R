# Flight Search GUI Application
# Shiny app for searching cheapest flights between cities

library(shiny)
library(rflights)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("✈️ Flight Search - Find Cheapest Flights"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Search Parameters"),
      
      textInput("departure", "Departure City/Country:", 
                value = "Canada", 
                placeholder = "e.g., Canada, Toronto, Paris"),
      
      textInput("arrival", "Arrival City:", 
                value = "Milan", 
                placeholder = "e.g., Milan, New York, Tokyo"),
      
      numericInput("days_ahead", "Days Ahead to Search:", 
                   value = 30, min = 1, max = 365),
      
      br(),
      actionButton("search", "🔍 Search Flights", 
                   class = "btn-primary", 
                   style = "width: 100%; font-size: 16px; padding: 10px;"),
      
      br(), br(),
      
      conditionalPanel(
        condition = "output.searching",
        h5("⏳ Searching...", style = "color: #007bff;")
      ),
      
      hr(),
      
      h5("Instructions:"),
      tags$ul(
        tags$li("Enter departure city or country"),
        tags$li("Enter arrival city"),
        tags$li("Click 'Search Flights'"),
        tags$li("Results sorted by price (cheapest first)")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.has_results",
        h3("Flight Results"),
        DT::dataTableOutput("flight_table"),
        br(),
        h4("Summary Statistics"),
        verbatimTextOutput("summary_stats")
      ),
      
      conditionalPanel(
        condition = "output.no_results",
        h4("No flights found."),
        p("This could mean:"),
        tags$ul(
          tags$li("No flights available in the date range"),
          tags$li("API returned empty results"),
          tags$li("Check your internet connection"),
          tags$li("Try different city names")
        )
      ),
      
      conditionalPanel(
        condition = "output.error",
        h4("Error occurred:"),
        verbatimTextOutput("error_message")
      )
    )
  ),
  
  tags$style("
    .btn-primary {
      background-color: #007bff;
      border-color: #007bff;
    }
    .btn-primary:hover {
      background-color: #0056b3;
      border-color: #0056b3;
    }
  ")
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  flight_data <- reactiveVal(NULL)
  searching <- reactiveVal(FALSE)
  has_error <- reactiveVal(FALSE)
  error_msg <- reactiveVal("")
  
  # Search button observer
  observeEvent(input$search, {
    searching(TRUE)
    has_error(FALSE)
    flight_data(NULL)
    
    tryCatch({
      # Find departure location
      dep_locs <- find_location(input$departure, location_types = c("CITY", "COUNTRY"))
      
      if (is.data.frame(dep_locs$node) && nrow(dep_locs$node) > 0) {
        dep_id <- dep_locs$node$id[1]
      } else {
        stop("Could not find departure location: ", input$departure)
      }
      
      # Find arrival location
      arr_locs <- find_location(input$arrival, location_types = c("CITY"))
      
      if (is.data.frame(arr_locs$node) && nrow(arr_locs$node) > 0) {
        # Filter for City type if available
        arr_cities <- arr_locs$node[arr_locs$node$typename == "City", ]
        if (nrow(arr_cities) > 0) {
          arr_id <- arr_cities$id[1]
        } else {
          arr_id <- arr_locs$node$id[1]
        }
      } else {
        stop("Could not find arrival location: ", input$arrival)
      }
      
      # Search for flights
      flights <- get_flights(
        fly_from = dep_id,
        fly_to = arr_id,
        departure_from = Sys.Date(),
        departure_to = Sys.Date() + input$days_ahead,
        sort_by = "PRICE"
      )
      
      # Process results
      if (!is.null(flights$status_code)) {
        stop("API request failed with status code: ", flights$status_code)
      } else if (is.data.frame(flights$itineraries) && nrow(flights$itineraries) > 0) {
        # Extract flight data
        source_stations <- character(nrow(flights$itineraries))
        dest_stations <- character(nrow(flights$itineraries))
        
        for (i in 1:nrow(flights$itineraries)) {
          sector_data <- flights$itineraries$sector[i, , drop = FALSE]
          if (ncol(sector_data) > 0 && !is.null(sector_data$sectorSegments)) {
            segs_list <- sector_data$sectorSegments[[1]]
            if (length(segs_list) > 0) {
              segs <- segs_list[[1]]
              if (!is.null(segs$segment) && length(segs$segment) > 0) {
                segments <- segs$segment
                if (length(segments) > 0 && !is.null(segments[[1]]$source$station$name)) {
                  source_stations[i] <- segments[[1]]$source$station$name
                }
                if (length(segments) > 0) {
                  last_seg <- segments[[length(segments)]]
                  if (!is.null(last_seg$destination$station$name)) {
                    dest_stations[i] <- last_seg$destination$station$name
                  }
                }
              }
            }
          }
        }
        
        # Fallback if stations are empty
        if (all(source_stations == "")) {
          source_stations <- rep(input$departure, nrow(flights$itineraries))
          dest_stations <- rep(input$arrival, nrow(flights$itineraries))
        }
        
        # Extract prices
        prices <- flights$itineraries$price$amount
        if (is.list(prices)) {
          prices <- unlist(prices)
        }
        prices <- as.numeric(prices)
        
        # Create summary data frame
        flight_summary <- data.frame(
          From = source_stations,
          To = dest_stations,
          Price_USD = round(prices, 2),
          Currency = if (!is.null(flights$itineraries$price$currency)) flights$itineraries$price$currency else "USD",
          stringsAsFactors = FALSE
        )
        
        # Sort by price
        flight_summary <- flight_summary[order(flight_summary$Price_USD), ]
        
        flight_data(flight_summary)
        searching(FALSE)
      } else {
        flight_data(NULL)
        searching(FALSE)
      }
    }, error = function(e) {
      has_error(TRUE)
      error_msg(paste("Error:", e$message))
      searching(FALSE)
      flight_data(NULL)
    })
  })
  
  # Outputs
  output$searching <- reactive({ searching() })
  outputOptions(output, "searching", suspendWhenHidden = FALSE)
  
  output$has_results <- reactive({ 
    !is.null(flight_data()) && nrow(flight_data()) > 0 && !searching()
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  
  output$no_results <- reactive({ 
    !is.null(flight_data()) && nrow(flight_data()) == 0 && !searching() && !has_error()
  })
  outputOptions(output, "no_results", suspendWhenHidden = FALSE)
  
  output$error <- reactive({ has_error() && !searching() })
  outputOptions(output, "error", suspendWhenHidden = FALSE)
  
  output$error_message <- renderText({ error_msg() })
  
  output$flight_table <- DT::renderDataTable({
    req(flight_data())
    df <- flight_data()
    table <- DT::datatable(
      df,
      options = list(
        pageLength = 20,
        order = list(list(2, 'asc')),  # Sort by price column
        scrollX = TRUE
      ),
      rownames = FALSE,
      colnames = c("From", "To", "Price (USD)", "Currency")
    )
    DT::formatCurrency(table, columns = "Price_USD", currency = "$", digits = 2)
  })
  
  output$summary_stats <- renderText({
    req(flight_data())
    df <- flight_data()
    paste(
      "Total flights found:", nrow(df), "\n",
      "Cheapest flight: $", round(min(df$Price_USD, na.rm = TRUE), 2), "\n",
      "Most expensive flight: $", round(max(df$Price_USD, na.rm = TRUE), 2), "\n",
      "Average price: $", round(mean(df$Price_USD, na.rm = TRUE), 2), "\n",
      "Median price: $", round(median(df$Price_USD, na.rm = TRUE), 2)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
