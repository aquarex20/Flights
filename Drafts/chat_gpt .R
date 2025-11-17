library(rflights)

country_origin  <- "Canada"
city_origin     <- "Montreal"
city_destination <- "Milan"
time_frame      <- 30  # next 30 days

## === Find origin ID ===
cat("=== Finding", city_origin, "location ID ===\n")
origin_locs <- find_location(city_origin, location_types = c("CITY"))

if (is.data.frame(origin_locs$node) && nrow(origin_locs$node) > 0) {
  print(origin_locs$node[, c("typename", "name", "id")])
  origin_id <- origin_locs$node$id[1]
  cat("\n✓ Using", city_origin, "ID:", origin_id, "\n\n")
} else {
  stop("Could not find ", city_origin, " location")
}

## === Get flights from origin (same as in your code) ===
flights <- search_flights(
  fly_from       = origin_id,
  departure_from = Sys.Date(),
  departure_to   = Sys.Date() + time_frame
)

itins <- flights$itineraries

## === Recursive search ===
find_cheapest_path_recursive <- function(itins,
                                         city_destination,
                                         max_depth = 3,
                                         min_price_init = Inf) {
  # best (global for the recursion)
  best_price <<- min_price_init
  best_path  <<- NULL
  
  # recursive DFS
  dfs <- function(level, total_price, path_so_far) {
    # stop if we already found something cheaper than any possible extension
    if (total_price > best_price) return(invisible(NULL))
    if (level > max_depth)        return(invisible(NULL))
    
    n <- nrow(itins)
    
    for (i in seq_len(n)) {
      dest  <- itins$destination$station$name[i]
      price <- as.numeric(itins$price$amount[i])
      new_total <- total_price + price
      if (new_total > best_price) next
      
      new_path <- c(path_so_far, dest)
      
      # ❗ base case: we "land" on our destination city
      if (identical(dest, city_destination)) {
        best_price <<- new_total
        best_path  <<- new_path
        # stop this branch here – we don't continue beyond our city
        next
      }
      
      # otherwise, go deeper (another leg) if depth allows
      dfs(level + 1, new_total, new_path)
    }
  }
  
  # start from level 1, price 0, empty path
  dfs(level = 1, total_price = 0, path_so_far = character(0))
  
  list(
    min_price = best_price,
    path      = best_path
  )
}

## Run the recursive search
res <- find_cheapest_path_recursive(
  itins            = itins,
  city_destination = city_destination,
  max_depth        = 3,      # like your triple loop
  min_price_init   = 1000    # like your min_price
)

res$min_price
res$path
