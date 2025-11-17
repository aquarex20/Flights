
library(rflights)
country_origin="Canada"
city_origin="Montreal"
city_destination="Milan"
time_frame=30 #in the next 30 days for example 

# Find your City or Country's location ID
cat("=== Finding", city_origin, " location ID ===\n")
origin_locs <- find_location(city_origin, location_types = c("CITY"))

if (is.data.frame(origin_locs$node) && nrow(origin_locs$node) > 0) {
  print(origin_locs$node[, c("typename", "name", "id")])
  origin_id <- origin_locs$node$id[1]
  cat("\n✓ Using", city_origin,"ID:", origin_id, "\n\n")
} else {
  stop("Could not find", city_origin," location")
}

nrow(flights$itineraries$price)

finsihed <- FALSE
level <- -1
new_origin_id <- origin_id
total_price <- 0
min_price_list <- list()
min_itinerary_list <- list()
min_price <- 1000

# 1) From origin
flights1 <- search_flights(
  fly_from       = new_origin_id,
  departure_from = Sys.Date(),
  departure_to   = Sys.Date() + time_frame
)

destination_sequence <- list()
min_destination_sequence <- list()

itins1 <- flights1$itineraries

for (i1 in seq_len(nrow(itins1))) {
  dest    <- itins1$destination$station$city$name[i1]
  dest_id <- itins1$destination$station$id[i1]
  if (is.na(dest)) next
  
  price1 <- as.numeric(itins1$price$amount[i1])
  if (is.na(price1)) next
  
  total_price1 <- total_price + price1
  print(total_price1)
  
  # PUSH level 1
  destination_sequence[[length(destination_sequence) + 1]] <- dest
  
  if (total_price1 > min_price) {
    # POP before skipping
    destination_sequence <- destination_sequence[-length(destination_sequence)]
    next
  }
  
  if (dest == city_destination) {
    min_price <- total_price1
    min_price_list[[length(min_price_list) + 1]] <- min_price
    min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
    min_destination_sequence <- destination_sequence
  }
  
  # 2) From dest_id
  flights2 <- search_flights(
    fly_from       = dest_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame
  )
  itins2 <- flights2$itineraries
  
  for (i2 in seq_len(nrow(itins2))) {
    dest2    <- itins2$destination$station$city$name[i2]
    dest_id2 <- itins2$destination$station$id[i2]
    if (is.na(dest2)) next
    
    price2 <- as.numeric(itins2$price$amount[i2])
    if (is.na(price2)) next
    
    total_price2 <- total_price1 + price2
    
    cat("\n[L2] total price1", total_price1)
    cat("\n[L2] destination of price 1", dest)
    cat("\n[L2] total price2", total_price2)
    cat("\n[L2] destination of price 2", dest2)
    
    # PUSH level 2
    destination_sequence[[length(destination_sequence) + 1]] <- dest2
    
    if (total_price2 > min_price) {
      # POP before skipping
      destination_sequence <- destination_sequence[-length(destination_sequence)]
      next
    }
    
    if (dest2 == city_destination) {
      min_price <- total_price2
      min_price_list[[length(min_price_list) + 1]] <- min_price
      min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
      min_destination_sequence <- destination_sequence
    }
    
    # 3) From dest_id2
    flights3 <- search_flights(
      fly_from       = dest_id2,
      departure_from = Sys.Date(),
      departure_to   = Sys.Date() + time_frame
    )
    itins3 <- flights3$itineraries
    
    for (i3 in seq_len(nrow(itins3))) {
      dest3    <- itins3$destination$station$city$name[i3]
      dest_id3 <- itins3$destination$station$id[i3]
      if (is.na(dest3)) next
      
      price3 <- as.numeric(itins3$price$amount[i3])
      if (is.na(price3)) next
      
      total_price3 <- total_price2 + price3
      
      cat("\n[L3] total price2", total_price2)
      cat("\n[L3] destination of price 2", dest2)
      cat("\n[L3] total price3", total_price3)
      cat("\n[L3] destination of price 3", dest3)
      
      # PUSH level 3
      destination_sequence[[length(destination_sequence) + 1]] <- dest3
      
      if (total_price3 > min_price) {
        # POP before skipping
        destination_sequence <- destination_sequence[-length(destination_sequence)]
        next
      }
      
      if (dest3 == city_destination) {
        min_price <- total_price3
        min_price_list[[length(min_price_list) + 1]] <- min_price
        min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
        min_destination_sequence <- destination_sequence
      }
      
      # 4) From dest_id3
      flights4 <- search_flights(
        fly_from       = dest_id3,
        departure_from = Sys.Date(),
        departure_to   = Sys.Date() + time_frame
      )
      itins4 <- flights4$itineraries
      
      for (i4 in seq_len(nrow(itins4))) {
        dest4    <- itins4$destination$station$city$name[i4]
        dest_id4 <- itins4$destination$station$id[i4]
        if (is.na(dest4)) next
        
        price4 <- as.numeric(itins4$price$amount[i4])
        if (is.na(price4)) next
        
        total_price4 <- total_price3 + price4
        
        cat("\n[L4] total price3", total_price3)
        cat("\n[L4] destination of price 3", dest3)
        cat("\n[L4] total price4", total_price4)
        cat("\n[L4] destination of price 4", dest4)
        
        # PUSH level 4
        destination_sequence[[length(destination_sequence) + 1]] <- dest4
        
        if (total_price4 > min_price) {
          # POP before skipping
          destination_sequence <- destination_sequence[-length(destination_sequence)]
          next
        }
        
        if (dest4 == city_destination) {
          min_price <- total_price4
          min_price_list[[length(min_price_list) + 1]] <- min_price
          min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
          min_destination_sequence <- destination_sequence
        }
        
        # POP level 4
        destination_sequence <- destination_sequence[-length(destination_sequence)]
      }
      
      # POP level 3
      destination_sequence <- destination_sequence[-length(destination_sequence)]
    }
    
    # POP level 2
    destination_sequence <- destination_sequence[-length(destination_sequence)]
  }
  
  # POP level 1
  destination_sequence <- destination_sequence[-length(destination_sequence)]
}

min_price
min_destination_sequence
min_price_list
min_itinerary_list
