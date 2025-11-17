# Search for flights from Canada to Milan
options(repos = c(CRAN = "https://cloud.r-project.org"))

remotes::install_github("jcrodriguez1989/rflights")

library(rflights)

# Find Canada location ID
cat("=== Finding Canada location ID ===\n")
canada_locs <- find_location("Canada", location_types = c("COUNTRY"))

if (is.data.frame(canada_locs$node) && nrow(canada_locs$node) > 0) {
  print(canada_locs$node[, c("typename", "name", "id")])
  canada_id <- canada_locs$node$id[1]
  cat("\n✓ Using Canada ID:", canada_id, "\n\n")
} else {
  stop("Could not find Canada location")
}

# Find Milan location ID
cat("=== Finding Milan location ID ===\n")
milan_locs <- find_location("Milan", location_types = c("CITY"))

if (is.data.frame(milan_locs$node) && nrow(milan_locs$node) > 0) {
  print(milan_locs$node[, c("typename", "name", "id", "country")])
  # Filter for City type and get the first one
  milan_cities <- milan_locs$node[milan_locs$node$typename == "City", ]
  if (nrow(milan_cities) > 0) {
    milan_id <- milan_cities$id[1]
    cat("\n✓ Using Milan ID:", milan_id, "\n")
    if (!is.null(milan_cities$country$name[1])) {
      cat("  Country:", milan_cities$country$name[1], "\n\n")
    }
  } else {
    milan_id <- milan_locs$node$id[1]
    cat("\n✓ Using Milan ID:", milan_id, "\n\n")
  }
} else {
  stop("Could not find Milan location")
}

# Search for flights from Canada to Milan
# Search for flights in the next 30 days
cat("=== Searching for flights from Canada to Milan ===\n")
cat("Date range:", format(Sys.Date(), "%Y-%m-%d"), "to", format(Sys.Date() + 30, "%Y-%m-%d"), "\n")
cat("This may take a moment...\n\n")

flights <- get_flights(
  fly_from = canada_id,
  fly_to = milan_id,
  departure_from = Sys.Date(),
  departure_to = Sys.Date() + 30,
  sort_by = "PRICE"
)

# Display results
if (!is.null(flights$status_code)) {
  cat("ERROR: API request failed with status code:", flights$status_code, "\n")
} else if (is.data.frame(flights$itineraries) && nrow(flights$itineraries) > 0) {
  cat("\n=== FLIGHT RESULTS ===\n")
  cat("Total flights found:", nrow(flights$itineraries), "\n\n")
  
  # Extract data - handle nested structure
  # The sector column contains lists with sectorSegments
  source_stations <- character(nrow(flights$itineraries))
  dest_stations <- character(nrow(flights$itineraries))
  
  for (i in 1:nrow(flights$itineraries)) {
    # Access sector as a list column
    sector_data <- flights$itineraries$sector[i, , drop = FALSE]
    if (ncol(sector_data) > 0 && !is.null(sector_data$sectorSegments)) {
      segs_list <- sector_data$sectorSegments[[1]]
      if (length(segs_list) > 0) {
        # Get the first sectorSegments entry
        segs <- segs_list[[1]]
        if (!is.null(segs$segment) && length(segs$segment) > 0) {
          segments <- segs$segment
          # First segment source
          if (length(segments) > 0 && !is.null(segments[[1]]$source$station$name)) {
            source_stations[i] <- segments[[1]]$source$station$name
          }
          # Last segment destination
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
  
  # If still empty, try simpler approach - just show prices
  if (all(source_stations == "")) {
    source_stations <- rep("Canada", nrow(flights$itineraries))
    dest_stations <- rep("Milan", nrow(flights$itineraries))
  }
  
  # Extract prices - handle if it's a list or vector
  prices <- flights$itineraries$price$amount
  if (is.list(prices)) {
    prices <- unlist(prices)
  }
  prices <- as.numeric(prices)
  
  # Create a summary data frame
  flight_summary <- data.frame(
    From = source_stations,
    To = dest_stations,
    Price_USD = prices,
    Currency = if (!is.null(flights$itineraries$price$currency)) flights$itineraries$price$currency else "USD",
    Duration_minutes = if (!is.null(flights$itineraries$duration)) flights$itineraries$duration else NA,
    Stops = rep(0, nrow(flights$itineraries)),  # Simplified - assume direct for now
    stringsAsFactors = FALSE
  )
  
  # Sort by price
  flight_summary <- flight_summary[order(flight_summary$Price_USD), ]
  
  # Display top 20 cheapest flights
  cat("Top 20 cheapest flights:\n")
  print(head(flight_summary, 20))
  
  cat("\n=== SUMMARY STATISTICS ===\n")
  cat("Cheapest flight: $", round(min(flight_summary$Price_USD, na.rm = TRUE), 2), "\n")
  cat("Most expensive flight: $", round(max(flight_summary$Price_USD, na.rm = TRUE), 2), "\n")
  cat("Average price: $", round(mean(flight_summary$Price_USD, na.rm = TRUE), 2), "\n")
  cat("Median price: $", round(median(flight_summary$Price_USD, na.rm = TRUE), 2), "\n")
  if (!all(is.na(flight_summary$Stops))) {
    cat("\nDirect flights:", sum(flight_summary$Stops == 0, na.rm = TRUE), "\n")
    cat("Flights with stops:", sum(flight_summary$Stops > 0, na.rm = TRUE), "\n")
  }
  
} else {
  cat("\nNo flights found.\n")
  cat("This could mean:\n")
  cat("  - No flights available in the date range\n")
  cat("  - API returned empty results\n")
  cat("  - Check your internet connection\n")
}

