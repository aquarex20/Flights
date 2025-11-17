library(shiny)
library(DT)

# Source the function files and preserve function names
# Save city functions before sourcing country functions (which have same names)
source("flight_haroun_ai_city_functions_3_4.R")
find_cheapest_flight_city_3_city <- find_cheapest_flight_city_3
find_cheapest_flight_city_4_city <- find_cheapest_flight_city_4

# Now source country functions (will overwrite the city versions)
source("flight_haroun_ai_country_3_4.R")
find_cheapest_flight_city_3_country <- find_cheapest_flight_city_3
find_cheapest_flight_city_4_country <- find_cheapest_flight_city_4

# UI
ui <- fluidPage(
  titlePanel("Flight Search GUI"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Search Parameters"),
      
      # Function selection
      selectInput("function_type", 
                  "Select Function Type:",
                  choices = list(
                    "City Functions (3 stops)" = "city_3",
                    "City Functions (4 stops)" = "city_4",
                    "Country Functions (3 stops)" = "country_3",
                    "Country Functions (4 stops)" = "country_4"
                  ),
                  selected = "city_3"),
      
      # Common parameters
      textInput("country_origin", 
                "Country Origin:", 
                value = "Canada"),
      
      textInput("city_origin", 
                "City Origin:", 
                value = "Montreal"),
      
      # Conditional UI for country functions
      conditionalPanel(
        condition = "input.function_type == 'country_3' || input.function_type == 'country_4'",
        textInput("country_destination", 
                  "Country Destination:", 
                  value = "Italy")
      ),
      
      # Conditional UI for city functions
      conditionalPanel(
        condition = "input.function_type == 'city_3' || input.function_type == 'city_4'",
        textInput("city_destination", 
                  "City Destination:", 
                  value = "Milan")
      ),
      
      numericInput("time_frame", 
                   "Time Frame (days):", 
                   value = 30, 
                   min = 1, 
                   max = 365),
      
      br(),
      actionButton("search_btn", "Search Flights", class = "btn-primary"),
      
      br(), br(),
      h5("Note:"),
      p("This may take several minutes to complete.")
    ),
    
    mainPanel(
      h4("Search Results"),
      
      # Status message
      verbatimTextOutput("status"),
      
      br(),
      
      # Results display
      h5("Combined Results (Price & Itinerary):"),
      DT::dataTableOutput("combined_table"),
      
      br(),
      
      h5("Minimum Price List:"),
      DT::dataTableOutput("price_list_table"),
      
      br(),
      
      h5("Minimum Itinerary List:"),
      DT::dataTableOutput("itinerary_list_table"),
      
      br(),
      
      h5("Best Result:"),
      verbatimTextOutput("best_result")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store results
  results <- reactiveValues(
    data = NULL,
    status = "Ready to search..."
  )
  
  # Observe search button click
  observeEvent(input$search_btn, {
    
    # Update status
    results$status <- "Searching... This may take a while."
    output$status <- renderText(results$status)
    
    # Disable button during search (using updateActionButton)
    updateActionButton(session, "search_btn", label = "Searching...", disabled = TRUE)
    
    tryCatch({
      # Call appropriate function based on selection
      if (input$function_type == "city_3") {
        result <- find_cheapest_flight_city_3_city(
          country_origin = input$country_origin,
          city_origin = input$city_origin,
          city_destination = input$city_destination,
          time_frame = input$time_frame
        )
      } else if (input$function_type == "city_4") {
        result <- find_cheapest_flight_city_4_city(
          country_origin = input$country_origin,
          city_origin = input$city_origin,
          city_destination = input$city_destination,
          time_frame = input$time_frame
        )
      } else if (input$function_type == "country_3") {
        result <- find_cheapest_flight_city_3_country(
          country_origin = input$country_origin,
          city_origin = input$city_origin,
          country_destination = input$country_destination,
          city_destination = input$city_destination,
          time_frame = input$time_frame
        )
      } else if (input$function_type == "country_4") {
        result <- find_cheapest_flight_city_4_country(
          country_origin = input$country_origin,
          city_origin = input$city_origin,
          country_destination = input$country_destination,
          city_destination = input$city_destination,
          time_frame = input$time_frame
        )
      }
      
      results$data <- result
      results$status <- paste("Search completed successfully!")
      
    }, error = function(e) {
      results$status <- paste("Error:", e$message)
      results$data <- NULL
    }, finally = {
      updateActionButton(session, "search_btn", label = "Search Flights", disabled = FALSE)
    })
  })
  
  # Display status
  output$status <- renderText({
    results$status
  })
  
  # Display combined table (price and itinerary together)
  output$combined_table <- DT::renderDataTable({
    if (is.null(results$data) || 
        is.null(results$data$min_price_list) || 
        is.null(results$data$min_itinerary_list)) {
      return(data.frame(Index = integer(), Price = numeric(), Itinerary = character()))
    }
    
    price_list <- results$data$min_price_list
    itinerary_list <- results$data$min_itinerary_list
    
    if (length(price_list) == 0 || length(itinerary_list) == 0) {
      return(data.frame(Index = integer(), Price = numeric(), Itinerary = character()))
    }
    
    # Ensure both lists have the same length
    min_len <- min(length(price_list), length(itinerary_list))
    price_list <- price_list[1:min_len]
    itinerary_list <- itinerary_list[1:min_len]
    
    # Convert itinerary list to strings
    itinerary_strings <- sapply(itinerary_list, function(x) {
      paste(unlist(x), collapse = " -> ")
    })
    
    # Create combined data frame
    combined_df <- data.frame(
      Index = 1:min_len,
      Price = unlist(price_list),
      Itinerary = itinerary_strings,
      stringsAsFactors = FALSE
    )
    
    dt <- DT::datatable(combined_df, 
                        options = list(pageLength = 10, scrollX = TRUE, order = list(list(1, 'asc'))),
                        rownames = FALSE)
    DT::formatCurrency(dt, columns = "Price", currency = "$", digits = 2)
  })
  
  # Display price list table
  output$price_list_table <- DT::renderDataTable({
    if (is.null(results$data) || is.null(results$data$min_price_list)) {
      return(data.frame(Index = integer(), Price = numeric()))
    }
    
    price_list <- results$data$min_price_list
    if (length(price_list) == 0) {
      return(data.frame(Index = integer(), Price = numeric()))
    }
    
    # Convert list to data frame
    price_df <- data.frame(
      Index = 1:length(price_list),
      Price = unlist(price_list)
    )
    
    DT::datatable(price_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Display itinerary list table
  output$itinerary_list_table <- DT::renderDataTable({
    if (is.null(results$data) || is.null(results$data$min_itinerary_list)) {
      return(data.frame(Index = integer(), Itinerary = character()))
    }
    
    itinerary_list <- results$data$min_itinerary_list
    if (length(itinerary_list) == 0) {
      return(data.frame(Index = integer(), Itinerary = character()))
    }
    
    # Convert list of character vectors to data frame
    itinerary_strings <- sapply(itinerary_list, function(x) {
      paste(unlist(x), collapse = " -> ")
    })
    
    itinerary_df <- data.frame(
      Index = 1:length(itinerary_strings),
      Itinerary = itinerary_strings
    )
    
    DT::datatable(itinerary_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Display best result
  output$best_result <- renderText({
    if (is.null(results$data)) {
      return("No results yet.")
    }
    
    result <- results$data
    best_itinerary <- paste(unlist(result$min_destination_sequence), collapse = " -> ")
    
    paste0(
      "Minimum Price: $", round(result$min_price, 2), "\n",
      "Best Itinerary: ", best_itinerary
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

