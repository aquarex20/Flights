# DFS Flight Path Search
# ======================
# 
# This script implements a depth-first search (DFS) algorithm to find flight paths
# from an origin country to a destination country. The algorithm:
# 
# 1. Starts from the origin country
# 2. Searches for all cities reachable from the origin (using "anywhere" search)
# 3. For each city, recursively searches for reachable cities
# 4. Tracks cumulative flight costs along each path
# 5. Prunes paths that exceed the maximum cost threshold
# 6. Stops when reaching the destination country or hitting depth/cost limits
# 
# Key features:
# - Cost-based pruning: Discards paths exceeding max_cost
# - Depth limiting: Prevents infinite recursion with max_depth
# - Visited tracking: Avoids revisiting cities to prevent cycles
# - Multiple paths: Finds all valid paths, sorted by cost
#
# Usage:
#   paths <- find_flight_paths("Canada", "Italy", max_cost = 2000, max_depth = 3)
#   print_paths(paths)

library(rflights)

# Helper function to extract destination cities and countries from flight results
extract_destinations <- function(flights_result) {
  if (is.null(flights_result) || 
      !is.data.frame(flights_result$itineraries) || 
      nrow(flights_result$itineraries) == 0) {
    return(data.frame(
      city_id = character(0),
      city_name = character(0),
      country_id = character(0),
      country_code = character(0),
      price = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  n <- nrow(flights_result$itineraries)
  destinations <- data.frame(
    city_id = character(n),
    city_name = character(n),
    country_id = character(n),
    country_code = character(n),
    price = numeric(n),
    stringsAsFactors = FALSE
  )
  
  # Extract prices
  prices <- flights_result$itineraries$price$amount
  if (is.list(prices)) {
    prices <- unlist(prices)
  }
  prices <- as.numeric(prices)
  
  # Extract destination information from each itinerary
  for (i in 1:n) {
    sector_data <- flights_result$itineraries$sector[i, , drop = FALSE]
    
    if (ncol(sector_data) > 0 && !is.null(sector_data$sectorSegments)) {
      segs_list <- sector_data$sectorSegments[[1]]
      
      if (length(segs_list) > 0) {
        segs <- segs_list[[1]]
        
        if (!is.null(segs$segment) && length(segs$segment) > 0) {
          segments <- segs$segment
          
          # Get the last segment's destination
          if (length(segments) > 0) {
            last_seg <- segments[[length(segments)]]
            
            # Extract destination station info
            if (!is.null(last_seg$destination$station)) {
              station <- last_seg$destination$station
              
              # City info
              if (!is.null(station$city)) {
                if (is.list(station$city) && length(station$city) > 0) {
                  if (!is.null(station$city$id)) {
                    destinations$city_id[i] <- station$city$id
                  }
                  if (!is.null(station$city$name)) {
                    destinations$city_name[i] <- station$city$name
                  }
                }
              }
              
              # Country info
              if (!is.null(station$country)) {
                if (is.list(station$country) && length(station$country) > 0) {
                  if (!is.null(station$country$id)) {
                    destinations$country_id[i] <- station$country$id
                  }
                  if (!is.null(station$country$code)) {
                    destinations$country_code[i] <- station$country$code
                  }
                }
              }
            }
          }
        }
      }
    }
    
    destinations$price[i] <- ifelse(is.na(prices[i]), Inf, prices[i])
  }
  
  # Remove rows with missing city_id (invalid destinations)
  destinations <- destinations[destinations$city_id != "", ]
  
  # Remove duplicates, keeping cheapest price for each city
  if (nrow(destinations) > 0) {
    destinations <- destinations[order(destinations$price), ]
    destinations <- destinations[!duplicated(destinations$city_id), ]
  }
  
  return(destinations)
}

# DFS function to find paths from origin to destination country
dfs_flight_search <- function(
  origin_country_name,
  destination_country_name,
  max_cost = Inf,
  max_depth = 5,
  departure_from = Sys.Date(),
  departure_to = Sys.Date() + 30,
  origin_country_id = NULL,
  destination_country_id = NULL,
  current_path = NULL,
  current_cost = 0,
  visited_cities = character(0),
  best_paths = list(),
  verbose = TRUE
) {
  
  # Initialize on first call
  if (is.null(current_path)) {
    if (verbose) {
      cat("=== DFS Flight Search ===\n")
      cat("Origin:", origin_country_name, "\n")
      cat("Destination:", destination_country_name, "\n")
      cat("Max Cost:", ifelse(is.infinite(max_cost), "Unlimited", paste("$", max_cost)), "\n")
      cat("Max Depth:", max_depth, "\n\n")
    }
    
    # Find origin country ID if not provided
    if (is.null(origin_country_id)) {
      origin_locs <- find_location(origin_country_name, location_types = c("COUNTRY"))
      if (!is.data.frame(origin_locs$node) || nrow(origin_locs$node) == 0) {
        stop("Could not find origin country: ", origin_country_name)
      }
      origin_country_id <- origin_locs$node$id[1]
    }
    
    # Find destination country ID if not provided
    if (is.null(destination_country_id)) {
      dest_locs <- find_location(destination_country_name, location_types = c("COUNTRY"))
      if (!is.data.frame(dest_locs$node) || nrow(dest_locs$node) == 0) {
        stop("Could not find destination country: ", destination_country_name)
      }
      destination_country_id <- dest_locs$node$id[1]
    }
    
    # Start search from origin country
    current_path <- list(list(
      location = origin_country_name,
      location_id = origin_country_id,
      location_type = "COUNTRY",
      cost = 0
    ))
    visited_cities <- character(0)
    best_paths <- list()
  }
  
  # Check depth limit
  if (length(current_path) > max_depth + 1) {
    if (verbose && length(current_path) == max_depth + 1) {
      cat("  [Depth limit reached]\n")
    }
    return(best_paths)
  }
  
  # Check cost limit
  if (current_cost > max_cost) {
    return(best_paths)
  }
  
  # Check if we're already in destination country (shouldn't happen, but safety check)
  if (length(current_path) > 1) {
    last_location <- current_path[[length(current_path)]]
    if (is.list(last_location) && !is.null(last_location$country_id) && last_location$country_id == destination_country_id) {
      # Already in destination country, but this should have been caught earlier
      return(best_paths)
    }
  }
  
  # Determine what to search from
  last_path_element <- current_path[[length(current_path)]]
  if (!is.list(last_path_element) || is.null(last_path_element$location_id)) {
    if (verbose) {
      cat("  Error: Invalid path element structure\n")
    }
    return(best_paths)
  }
  
  # If we're starting from a country, we need to find cities within that country first
  if (!is.null(last_path_element$location_type) && last_path_element$location_type == "COUNTRY") {
    if (verbose && length(current_path) == 1) {
      cat("Searching flights from:", last_path_element$location, "\n")
      cat("  Finding cities in", last_path_element$location, "...\n")
    }
    
    # Find cities within the country
    # Try searching for the country name to find cities
    country_cities <- find_location(last_path_element$location, location_types = c("CITY"), first = 50)
    
    if (is.null(country_cities$status_code) && is.data.frame(country_cities$node) && nrow(country_cities$node) > 0) {
      # Filter cities that belong to this country
      city_nodes <- country_cities$node
      # Check if cities have country information
      if ("country" %in% colnames(city_nodes) && !is.null(city_nodes$country)) {
        # Filter by country ID
        if (is.list(city_nodes$country)) {
          country_ids <- sapply(city_nodes$country, function(x) if(is.list(x) && !is.null(x$id)) x$id else NA)
          city_nodes <- city_nodes[!is.na(country_ids) & country_ids == last_path_element$location_id, ]
        }
      }
      
      if (nrow(city_nodes) > 0) {
        if (verbose && length(current_path) == 1) {
          cat("  Found", nrow(city_nodes), "cities. Searching from each city...\n")
        }
        
        # Limit number of cities to search from to avoid too many API calls
        max_cities <- min(10, nrow(city_nodes))
        city_nodes <- city_nodes[1:max_cities, ]
        
        # Search from each city
        for (j in 1:nrow(city_nodes)) {
          city_id <- city_nodes$id[j]
          city_name <- city_nodes$name[j]
          
          # Create a new path starting from this city
          city_path <- list(list(
            location = city_name,
            location_id = city_id,
            location_type = "CITY",
            cost = 0
          ))
          
          # Recursively search from this city
          best_paths <- dfs_flight_search(
            origin_country_name = origin_country_name,
            destination_country_name = destination_country_name,
            max_cost = max_cost,
            max_depth = max_depth,
            departure_from = departure_from,
            departure_to = departure_to,
            origin_country_id = origin_country_id,
            destination_country_id = destination_country_id,
            current_path = city_path,
            current_cost = current_cost,
            visited_cities = visited_cities,
            best_paths = best_paths,
            verbose = verbose
          )
        }
        
        return(best_paths)
      }
    }
    
    # If we couldn't find cities through find_location, try searching for major cities by name
    # Define major cities for common countries
    major_cities <- list(
      "Canada" = c("Toronto", "Vancouver", "Montreal", "Calgary", "Ottawa", "Edmonton", "Winnipeg", "Quebec"),
      "Italy" = c("Rome", "Milan", "Venice", "Florence", "Naples", "Turin", "Bologna", "Genoa"),
      "United States" = c("New York", "Los Angeles", "Chicago", "Miami", "San Francisco", "Boston", "Seattle", "Washington"),
      "France" = c("Paris", "Lyon", "Marseille", "Nice", "Toulouse", "Bordeaux", "Strasbourg", "Nantes"),
      "Germany" = c("Berlin", "Munich", "Frankfurt", "Hamburg", "Cologne", "Stuttgart", "Düsseldorf", "Dresden"),
      "Spain" = c("Madrid", "Barcelona", "Valencia", "Seville", "Bilbao", "Malaga", "Granada", "Alicante"),
      "United Kingdom" = c("London", "Manchester", "Edinburgh", "Birmingham", "Liverpool", "Glasgow", "Bristol", "Leeds"),
      "Australia" = c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Gold Coast", "Newcastle", "Canberra"),
      "Japan" = c("Tokyo", "Osaka", "Kyoto", "Yokohama", "Sapporo", "Fukuoka", "Hiroshima", "Nagoya"),
      "China" = c("Beijing", "Shanghai", "Guangzhou", "Shenzhen", "Chengdu", "Hangzhou", "Xi'an", "Nanjing")
    )
    
    country_name <- last_path_element$location
    cities_to_try <- major_cities[[country_name]]
    
    if (!is.null(cities_to_try) && length(cities_to_try) > 0) {
      if (verbose && length(current_path) == 1) {
        cat("  Searching for major cities:", paste(cities_to_try[1:min(5, length(cities_to_try))], collapse = ", "), "...\n")
      }
      
      found_cities <- list()
      max_cities_to_find <- 10
      
      for (city_name in cities_to_try) {
        if (length(found_cities) >= max_cities_to_find) break
        
        city_results <- find_location(city_name, location_types = c("CITY"), first = 5)
        
        if (is.null(city_results$status_code) && is.data.frame(city_results$node) && nrow(city_results$node) > 0) {
          city_nodes <- city_results$node
          
          # Filter cities that belong to this country
          if ("country" %in% colnames(city_nodes) && !is.null(city_nodes$country)) {
            if (is.list(city_nodes$country)) {
              country_ids <- sapply(city_nodes$country, function(x) {
                if (is.list(x) && !is.null(x$id)) x$id else NA
              })
              matching_cities <- city_nodes[!is.na(country_ids) & country_ids == last_path_element$location_id, ]
              
              if (nrow(matching_cities) > 0) {
                # Add unique cities
                for (k in 1:nrow(matching_cities)) {
                  city_id <- matching_cities$id[k]
                  if (!city_id %in% sapply(found_cities, function(x) x$id)) {
                    found_cities[[length(found_cities) + 1]] <- list(
                      id = city_id,
                      name = matching_cities$name[k]
                    )
                  }
                }
              }
            }
          }
        }
        
        # Small delay to avoid rate limiting
        Sys.sleep(0.3)
      }
      
      if (length(found_cities) > 0) {
        if (verbose && length(current_path) == 1) {
          cat("  Found", length(found_cities), "cities. Searching from each city...\n")
        }
        
        # Search from each found city
        for (city_info in found_cities) {
          city_path <- list(list(
            location = city_info$name,
            location_id = city_info$id,
            location_type = "CITY",
            cost = 0
          ))
          
          # Recursively search from this city
          best_paths <- dfs_flight_search(
            origin_country_name = origin_country_name,
            destination_country_name = destination_country_name,
            max_cost = max_cost,
            max_depth = max_depth,
            departure_from = departure_from,
            departure_to = departure_to,
            origin_country_id = origin_country_id,
            destination_country_id = destination_country_id,
            current_path = city_path,
            current_cost = current_cost,
            visited_cities = visited_cities,
            best_paths = best_paths,
            verbose = verbose
          )
        }
        
        return(best_paths)
      }
    }
    
    # If we still couldn't find cities, try searching directly from the country ID
    if (verbose && length(current_path) == 1) {
      cat("  Could not find cities, trying to search from country ID directly...\n")
    }
  }
  
  search_from_id <- last_path_element$location_id
  
  if (verbose && length(current_path) == 1) {
    first_elem <- current_path[[1]]
    loc_name <- if (is.list(first_elem) && !is.null(first_elem$location)) first_elem$location else "unknown"
    cat("Searching flights from:", loc_name, "\n")
  }
  
  # Search for flights to anywhere from current location
  tryCatch({
    # Small delay to avoid rate limiting (only for recursive calls)
    if (length(current_path) > 1) {
      Sys.sleep(0.5)
    }
    
    flights <- search_flights(
      fly_from = search_from_id,
      fly_to = "anywhere",
      departure_from = departure_from,
      departure_to = departure_to,
      sort_by = "PRICE"
    )
    
    # Check for API errors
    if (!is.null(flights$status_code)) {
      if (verbose) {
        cat("  API error:", flights$status_code, "\n")
      }
      return(best_paths)
    }
    
    # Extract destinations
    destinations <- extract_destinations(flights)
    
    if (nrow(destinations) == 0) {
      if (verbose && length(current_path) == 1) {
        cat("  No flights found\n")
      }
      return(best_paths)
    }
    
    if (verbose && length(current_path) == 1) {
      cat("  Found", nrow(destinations), "reachable cities\n")
    }
    
    # Limit number of destinations to process per level to avoid too many API calls
    # Process cheapest destinations first
    max_destinations_per_level <- min(20, nrow(destinations))
    destinations <- destinations[1:max_destinations_per_level, ]
    
    if (verbose && length(current_path) > 1) {
      last_elem <- current_path[[length(current_path)]]
      loc_name <- if (is.list(last_elem) && !is.null(last_elem$location)) last_elem$location else "unknown"
      cat("  [Depth ", length(current_path) - 1, "] Exploring ", nrow(destinations), 
          " destinations from ", loc_name, "\n", sep = "")
    }
    
    # Process each destination
    for (i in 1:nrow(destinations)) {
      dest_city_id <- destinations$city_id[i]
      dest_city_name <- destinations$city_name[i]
      dest_country_id <- destinations$country_id[i]
      dest_price <- destinations$price[i]
      new_cost <- current_cost + dest_price
      
      # Skip if exceeds cost limit
      if (new_cost > max_cost) {
        next
      }
      
      # Skip if already visited this city
      if (dest_city_id %in% visited_cities) {
        next
      }
      
      # Create new path
      new_path <- c(current_path, list(list(
        location = dest_city_name,
        location_id = dest_city_id,
        location_type = "CITY",
        country_id = dest_country_id,
        cost = dest_price,
        cumulative_cost = new_cost
      )))
      
      # Check if we reached destination country
      if (dest_country_id == destination_country_id) {
        # Found a path!
        if (verbose) {
          cat("\n✓ Found path (cost: $", round(new_cost, 2), "):\n", sep = "")
          for (j in 1:length(new_path)) {
            if (j == 1) {
              cat("  ", new_path[[j]]$location, " [", new_path[[j]]$location_type, "]\n", sep = "")
            } else {
              cat("  -> ", new_path[[j]]$location, " [", new_path[[j]]$location_type, 
                  "] ($", round(new_path[[j]]$cost, 2), ")\n", sep = "")
            }
          }
          cat("\n")
        }
        
        # Add to best paths
        best_paths[[length(best_paths) + 1]] <- list(
          path = new_path,
          total_cost = new_cost,
          depth = length(new_path) - 1
        )
        
        # Continue searching for other paths (don't return early)
      } else {
        # Continue DFS recursively
        new_visited <- c(visited_cities, dest_city_id)
        
        best_paths <- dfs_flight_search(
          origin_country_name = origin_country_name,
          destination_country_name = destination_country_name,
          max_cost = max_cost,
          max_depth = max_depth,
          departure_from = departure_from,
          departure_to = departure_to,
          origin_country_id = origin_country_id,
          destination_country_id = destination_country_id,
          current_path = new_path,
          current_cost = new_cost,
          visited_cities = new_visited,
          best_paths = best_paths,
          verbose = verbose
        )
      }
    }
    
  }, error = function(e) {
    if (verbose) {
      cat("  Error:", e$message, "\n")
    }
  })
  
  return(best_paths)
}

# Wrapper function with better interface
find_flight_paths <- function(
  origin_country,
  destination_country,
  max_cost = Inf,
  max_depth = 5,
  days_ahead = 30,
  verbose = TRUE
) {
  departure_from <- Sys.Date()
  departure_to <- Sys.Date() + days_ahead
  
  paths <- dfs_flight_search(
    origin_country_name = origin_country,
    destination_country_name = destination_country,
    max_cost = max_cost,
    max_depth = max_depth,
    departure_from = departure_from,
    departure_to = departure_to,
    origin_country_id = NULL,
    destination_country_id = NULL,
    verbose = verbose
  )
  
  # Sort paths by cost
  if (length(paths) > 0) {
    costs <- sapply(paths, function(p) p$total_cost)
    paths <- paths[order(costs)]
  }
  
  return(paths)
}

# Function to format and display results
print_paths <- function(paths) {
  if (length(paths) == 0) {
    cat("No paths found.\n")
    return(invisible(NULL))
  }
  
  cat("\n=== Found", length(paths), "path(s) ===\n\n")
  
  for (i in 1:length(paths)) {
    path <- paths[[i]]
    cat("Path", i, "- Total Cost: $", round(path$total_cost, 2), 
        " (", path$depth, " flights)\n", sep = "")
    
    for (j in 1:length(path$path)) {
      step <- path$path[[j]]
      if (j == 1) {
        cat("  Start: ", step$location, " [", step$location_type, "]\n", sep = "")
      } else {
        cat("  Flight ", j-1, ": ", path$path[[j-1]]$location, 
            " -> ", step$location, " [", step$location_type, "]",
            " ($", round(step$cost, 2), ")\n", sep = "")
      }
    }
    cat("\n")
  }
}

# Example usage:

# Find paths from Canada to Italy with max cost of $2000 and max 3 flights
paths <- find_flight_paths(
  origin_country = "Canada",
  destination_country = "Italy", 
  max_cost = 2000,
  max_depth = 3,
  days_ahead = 30,
  verbose = TRUE
)

# Print all found paths
print_paths(paths)

# Access individual paths
if (length(paths) > 0) {
  cheapest_path <- paths[[1]]
  cat("Cheapest path cost: $", cheapest_path$total_cost, "\n")
  cat("Number of flights: ", cheapest_path$depth, "\n")
}

