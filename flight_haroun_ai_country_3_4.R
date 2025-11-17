
library(rflights)
library(geodist)

find_cheapest_flight_city_4 <-function(country_origin="Canada", city_origin="Montreal",country_destination="Italy", city_destination="Milan", time_frame=30){
  # Find your City or Country's location ID
  cat("=== Finding", city_origin, " location ID ===\n")
  origin_locs <- find_location(city_origin, location_types = c("CITY"))
  destination_locs <- find_location(country_destination, location_types = c("COUNTRY"))
  
  if (is.data.frame(origin_locs$node) && nrow(origin_locs$node) > 0) {
    print(origin_locs$node[, c("typename", "name", "id")])
    origin_id <- origin_locs$node$id[1]
    cat("\n✓ Using", city_origin,"ID:", origin_id, "\n\n")
  } else {
    stop("Could not find", city_origin," location")
  }
  if (is.data.frame(destination_locs$node) && nrow(destination_locs$node) > 0) {
    print(destination_locs$node[, c("typename", "name", "id")])
    destination_id <- destination_locs$node$id[1]
    cat("\n✓ Using", country_destination,"ID:", destination_id, "\n\n")
  } else {
    stop("Could not find", country_destination," location")
  }
  
  nrow(flights$itineraries$price)
  
  further_away_than_before <- function(itins1, itins2, i1, i2) {
    # previous origin
    origin <- list(
      lon = itins1$source$station$city$gps$lng[i1],
      lat = itins1$source$station$city$gps$lat[i1]
    )
    
    # new intermediate destination
    destination <- list(
      lon = itins2$source$station$city$gps$lng[i2],
      lat = itins2$source$station$city$gps$lat[i2]
    )
    
    # final destination (same itinerary 2)
    final_destination <- list(
      lon = itins2$destination$station$city$gps$lng[i2],
      lat = itins2$destination$station$city$gps$lat[i2]
    )
    
    # origin -> final
    df1 <- data.frame(
      lon = c(origin$lon, final_destination$lon),
      lat = c(origin$lat, final_destination$lat)
    )
    dmat1 <- geodist(df1, measure = "haversine")
    dist1 <- as.numeric(dmat1[1, 2])  # single number
    
    # destination -> final
    df2 <- data.frame(
      lon = c(destination$lon, final_destination$lon),
      lat = c(destination$lat, final_destination$lat)
    )
    dmat2 <- geodist(df2, measure = "haversine")
    dist2 <- as.numeric(dmat2[1, 2])  # single number
    
    # return single TRUE/FALSE
    return(dist2 > dist1)
  }
  
  
  find_lowest_price <- function(origin_id, destination_id, departure_from, time_frame) {
    flights <- search_flights(
      fly_from       = origin_id,
      departure_from,
      departure_to=departure_from+time_frame,
      fly_to= destination_id
    )
    numeric_prices <- as.numeric(flights$itineraries$price$amount)
    min_value <- min(numeric_prices, na.rm = TRUE)
    return(min_value)
  }
  
  finsihed <- FALSE
  level <- -1
  new_origin_id <- origin_id
  total_price <- 0
  min_price_list <- list()
  min_itinerary_list <- list()
  min_price <- find_lowest_price(origin_id, destination_id, Sys.Date(), time_frame)
  cat("We start with a min price of ", min_price)
  
  flight_base <- search_flights(
    fly_from       = new_origin_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame,
    fly_to= destination_id
  )
  
  
  # 1) From origin
  flights1 <- search_flights(
    fly_from       = new_origin_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame
  )
  
  destination_sequence <- list()
  min_destination_sequence <- list()
  
  itins1 <- flights1$itineraries
  reachable_destinations1 <- as.list(itins1$destination$station$city$name)
  
  for (i1 in seq_len(nrow(itins1))) {
    dest    <- itins1$destination$station$city$name[i1]
    country    <- itins1$destination$station$city$country$name[i1]
    dest_id <- itins1$destination$station$id[i1]
    if (is.na(dest)) next
    
    price1 <- as.numeric(itins1$price$amount[i1])
    if (is.na(price1)) next
    
    price1=find_lowest_price(origin_id = new_origin_id, 
                             destination_id = dest_id, 
                             departure_from = Sys.Date(), 
                             time_frame = time_frame)
    
    total_price1 <- total_price + price1
    print(total_price1)
    
    # PUSH level 1
    destination_sequence[[length(destination_sequence) + 1]] <- dest
    
    if (total_price1 > min_price) {
      # POP before skipping
      destination_sequence <- destination_sequence[-length(destination_sequence)]
      next
    }
    
    if (country == country_destination) {
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
      country2    <- itins2$destination$station$city$country$name[i2]
      if (is.na(dest2)) next
      
      price2 <- as.numeric(itins2$price$amount[i2])
      if (is.na(price2)) next
      
      price2=find_lowest_price(origin_id = dest_id, 
                               destination_id = dest_id2, 
                               departure_from = Sys.Date(), 
                               time_frame = time_frame)
      
      
      total_price2 <- total_price1 + price2
      
      # PUSH level 2
      destination_sequence[[length(destination_sequence) + 1]] <- dest2
      
      if (total_price2 > min_price) {
        # POP before skipping
        destination_sequence <- destination_sequence[-length(destination_sequence)]
        next
      }
      
      if (country2 == country_destination) {
        min_price <- total_price2
        min_price_list[[length(min_price_list) + 1]] <- min_price
        min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
        min_destination_sequence <- destination_sequence
      }
      
      #3 
      flights3 <- search_flights(
        fly_from       = dest_id2,
        departure_from = Sys.Date(),
        departure_to   = Sys.Date() + time_frame
      )
      itins3 <- flights3$itineraries
      
      for (i3 in seq_len(nrow(itins3))) {
        dest3    <- itins3$destination$station$city$name[i3]
        country3    <- itins3$destination$station$city$country$name[i3]
        dest_id3 <- itins3$destination$station$id[i3]
        if (is.na(dest3)) next
        
        
        if (further_away_than_before(itins1, itins2, i1, i2 ))
          next
        
        price3 <- as.numeric(itins3$price$amount[i3])
        if (is.na(price3)) next
        
        price3=find_lowest_price(origin_id = dest_id, 
                                 destination_id = dest_id3, 
                                 departure_from = Sys.Date(), 
                                 time_frame = time_frame)
        
        
        total_price3 <- total_price2 + price3
        
        # PUSH level 3
        destination_sequence[[length(destination_sequence) + 1]] <- dest3
        
        if (total_price3 > min_price) {
          # POP before skipping
          destination_sequence <- destination_sequence[-length(destination_sequence)]
          next
        }
        
        if (country3 == country_destination) {
          min_price <- total_price3
          min_price_list[[length(min_price_list) + 1]] <- min_price
          min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
          min_destination_sequence <- destination_sequence
        }
        #4 
        flights4 <- search_flights(
          fly_from       = dest_id3,
          departure_from = Sys.Date(),
          departure_to   = Sys.Date() + time_frame
        )
        itins4 <- flights4$itineraries
        
        for (i4 in seq_len(nrow(itins4))) {
          dest4    <- itins4$destination$station$city$name[i4]
          country4    <- itins4$destination$station$city$country$name[i4]
          
          dest_id4 <- itins4$destination$station$id[i4]
          if (is.na(dest4)) next
          
          
          if (further_away_than_before(itins2, itins3, i2, i3 ))
            next
          
          price4 <- as.numeric(itins4$price$amount[i4])
          if (is.na(price4)) next
          
          price4=find_lowest_price(origin_id = dest_id, 
                                   destination_id = dest_id4, 
                                   departure_from = Sys.Date(), 
                                   time_frame = time_frame)
          
          
          total_price4 <- total_price3 + price4
          
          # PUSH level 4
          destination_sequence[[length(destination_sequence) + 1]] <- dest4
          
          if (total_price4 > min_price) {
            # POP before skipping
            destination_sequence <- destination_sequence[-length(destination_sequence)]
            next
          }
          
          if (country4 == country_destination) {
            min_price <- total_price4
            min_price_list[[length(min_price_list) + 1]] <- min_price
            min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
            min_destination_sequence <- destination_sequence
          }
          
          #POP level 4
          destination_sequence <- destination_sequence[-length(destination_sequence)]
          
        }
        
        #POP level 3
        destination_sequence <- destination_sequence[-length(destination_sequence)]
        
      }
      
      # POP level 2
      destination_sequence <- destination_sequence[-length(destination_sequence)]
    }
    
    # POP level 1
    destination_sequence <- destination_sequence[-length(destination_sequence)]
  }
  return (list(min_price = min_price, min_destination_sequence = min_destination_sequence, min_price_list = min_price_list, min_itinerary_list = min_itinerary_list))
}

find_cheapest_flight_city_3 <-function(country_origin="Canada", city_origin="Montreal",country_destination="Italy", city_destination="Milan", time_frame=30){
  # Find your City or Country's location ID
  cat("=== Finding", city_origin, " location ID ===\n")
  origin_locs <- find_location(city_origin, location_types = c("CITY"))
  destination_locs <- find_location(country_destination, location_types = c("COUNTRY"))
  
  if (is.data.frame(origin_locs$node) && nrow(origin_locs$node) > 0) {
    print(origin_locs$node[, c("typename", "name", "id")])
    origin_id <- origin_locs$node$id[1]
    cat("\n✓ Using", city_origin,"ID:", origin_id, "\n\n")
  } else {
    stop("Could not find", city_origin," location")
  }
  if (is.data.frame(destination_locs$node) && nrow(destination_locs$node) > 0) {
    print(destination_locs$node[, c("typename", "name", "id")])
    destination_id <- destination_locs$node$id[1]
    cat("\n✓ Using", country_destination,"ID:", destination_id, "\n\n")
  } else {
    stop("Could not find", country_destination," location")
  }
  
  nrow(flights$itineraries$price)
  
  further_away_than_before <- function(itins1, itins2, i1, i2) {
    # previous origin
    origin <- list(
      lon = itins1$source$station$city$gps$lng[i1],
      lat = itins1$source$station$city$gps$lat[i1]
    )
    
    # new intermediate destination
    destination <- list(
      lon = itins2$source$station$city$gps$lng[i2],
      lat = itins2$source$station$city$gps$lat[i2]
    )
    
    # final destination (same itinerary 2)
    final_destination <- list(
      lon = itins2$destination$station$city$gps$lng[i2],
      lat = itins2$destination$station$city$gps$lat[i2]
    )
    
    # origin -> final
    df1 <- data.frame(
      lon = c(origin$lon, final_destination$lon),
      lat = c(origin$lat, final_destination$lat)
    )
    dmat1 <- geodist(df1, measure = "haversine")
    dist1 <- as.numeric(dmat1[1, 2])  # single number
    
    # destination -> final
    df2 <- data.frame(
      lon = c(destination$lon, final_destination$lon),
      lat = c(destination$lat, final_destination$lat)
    )
    dmat2 <- geodist(df2, measure = "haversine")
    dist2 <- as.numeric(dmat2[1, 2])  # single number
    
    # return single TRUE/FALSE
    return(dist2 > dist1)
  }
  
  
  find_lowest_price <- function(origin_id, destination_id, departure_from, time_frame) {
    flights <- search_flights(
      fly_from       = origin_id,
      departure_from,
      departure_to=departure_from+time_frame,
      fly_to= destination_id
    )
    numeric_prices <- as.numeric(flights$itineraries$price$amount)
    min_value <- min(numeric_prices, na.rm = TRUE)
    return(min_value)
  }
  
  finsihed <- FALSE
  level <- -1
  new_origin_id <- origin_id
  total_price <- 0
  min_price_list <- list()
  min_itinerary_list <- list()
  min_price <- find_lowest_price(origin_id, destination_id, Sys.Date(), time_frame)
  cat("We start with a min price of ", min_price)
  
  flight_base <- search_flights(
    fly_from       = new_origin_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame,
    fly_to= destination_id
  )
  
  
  # 1) From origin
  flights1 <- search_flights(
    fly_from       = new_origin_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame
  )
  
  destination_sequence <- list()
  min_destination_sequence <- list()
  
  itins1 <- flights1$itineraries
  reachable_destinations1 <- as.list(itins1$destination$station$city$name)
  
  for (i1 in seq_len(nrow(itins1))) {
    dest    <- itins1$destination$station$city$name[i1]
    country    <- itins1$destination$station$city$country$name[i1]
    dest_id <- itins1$destination$station$id[i1]
    if (is.na(dest)) next
    
    price1 <- as.numeric(itins1$price$amount[i1])
    if (is.na(price1)) next
    
    price1=find_lowest_price(origin_id = new_origin_id, 
                             destination_id = dest_id, 
                             departure_from = Sys.Date(), 
                             time_frame = time_frame)
    
    total_price1 <- total_price + price1
    print(total_price1)
    
    # PUSH level 1
    destination_sequence[[length(destination_sequence) + 1]] <- dest
    
    if (total_price1 > min_price) {
      # POP before skipping
      destination_sequence <- destination_sequence[-length(destination_sequence)]
      next
    }
    
    if (country == country_destination) {
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
      country2    <- itins2$destination$station$city$country$name[i2]
      if (is.na(dest2)) next
      
      price2 <- as.numeric(itins2$price$amount[i2])
      if (is.na(price2)) next
      
      price2=find_lowest_price(origin_id = dest_id, 
                               destination_id = dest_id2, 
                               departure_from = Sys.Date(), 
                               time_frame = time_frame)
      
      
      total_price2 <- total_price1 + price2
      
      # PUSH level 2
      destination_sequence[[length(destination_sequence) + 1]] <- dest2
      
      if (total_price2 > min_price) {
        # POP before skipping
        destination_sequence <- destination_sequence[-length(destination_sequence)]
        next
      }
      
      if (country2 == country_destination) {
        min_price <- total_price2
        min_price_list[[length(min_price_list) + 1]] <- min_price
        min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
        min_destination_sequence <- destination_sequence
      }
      
      #3 
      flights3 <- search_flights(
        fly_from       = dest_id2,
        departure_from = Sys.Date(),
        departure_to   = Sys.Date() + time_frame
      )
      itins3 <- flights3$itineraries
      
      for (i3 in seq_len(nrow(itins3))) {
        dest3    <- itins3$destination$station$city$name[i3]
        country3    <- itins3$destination$station$city$country$name[i3]
        dest_id3 <- itins3$destination$station$id[i3]
        if (is.na(dest3)) next
        
        
        if (further_away_than_before(itins1, itins2, i1, i2 ))
          next
        
        price3 <- as.numeric(itins3$price$amount[i3])
        if (is.na(price3)) next
        
        price3=find_lowest_price(origin_id = dest_id, 
                                 destination_id = dest_id3, 
                                 departure_from = Sys.Date(), 
                                 time_frame = time_frame)
        
        
        total_price3 <- total_price2 + price3
        
        # PUSH level 3
        destination_sequence[[length(destination_sequence) + 1]] <- dest3
        
        if (total_price3 > min_price) {
          # POP before skipping
          destination_sequence <- destination_sequence[-length(destination_sequence)]
          next
        }
        
        if (country3 == country_destination) {
          min_price <- total_price3
          min_price_list[[length(min_price_list) + 1]] <- min_price
          min_itinerary_list[[length(min_itinerary_list) + 1]] <- destination_sequence
          min_destination_sequence <- destination_sequence
        }
        
        #POP level 3
        destination_sequence <- destination_sequence[-length(destination_sequence)]
        
      }
      
      # POP level 2
      destination_sequence <- destination_sequence[-length(destination_sequence)]
    }
    
    # POP level 1
    destination_sequence <- destination_sequence[-length(destination_sequence)]
  }
  
  return (list(min_price = min_price, min_destination_sequence = min_destination_sequence, min_price_list = min_price_list, min_itinerary_list = min_itinerary_list))
}
