
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

finsihed=FALSE
level=-1
new_origin_id=origin_id
total_price=0
total_price1=0
total_price2=0
total_price3=0
min_price_list=list()
min_itinerary_list=list()
min_price=1000

#1
flights <- search_flights(
  fly_from       = new_origin_id,
  departure_from = Sys.Date(),
  departure_to   = Sys.Date() + time_frame,
)
flights1=flights
destination_sequence=list()
min_destination_sequence=list()

for (i in seq_len(nrow(flights$itineraries))) {
  dest  <- flights$itineraries$destination$station$city$name[i]
  dest_id  <- flights$itineraries$destination$station$id[i]
  if (is.na(dest)) next
  
  #flights <- search_flights(
  #  fly_from       = new_origin_id,
  #  departure_from = Sys.Date(),
  #  departure_to   = Sys.Date() + time_frame,
  #  fly_to = dest_id
  #)
  
  #find the minimum price from your origin to your destination because when you ask for destinations reachable from an origin it doesnt give you every possibility of getting there, just the possibilities. 
  price <- flights$itineraries$price$amount[i]
  cat("\nprice1:", price)
  cat("\ndest:", dest)
  cat("\norigin:", new_origin_id)
  
  total_price1=total_price+as.numeric(price)
  destination_sequence[length(destination_sequence)+1]<-dest

  if (total_price1>min_price){
    destination_sequence <- destination_sequence[-length(destination_sequence)]
  
    next
    }
  if (dest==city_destination){
    min_price=total_price1
    min_price_list[length(min_price_list)+1]<-min_price
    min_itinerary_list[length(min_itinerary_list)+1]<-destination_sequence
    min_destination_sequence=destination_sequence
  }
  #2
  flights <- search_flights(
    fly_from       = dest_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame,
  )
  flights2=flights
  
  for (i in seq_len(nrow(flights$itineraries))) {
    dest2  <- flights$itineraries$destination$station$city$name[i]
    dest_id2  <- flights$itineraries$destination$station$id[i]
    if (is.na(dest2)) next
    
    price <- flights$itineraries$price$amount[i]
    total_price2=total_price1+as.numeric(price)
    
    destination_sequence[length(destination_sequence)+1]<-dest2
    
    if (total_price2>min_price){      destination_sequence <- destination_sequence[-length(destination_sequence)]

    next}
    if (dest2==city_destination){
      min_price=total_price2
      min_price_list[length(min_price_list)+1]<-min_price
      min_itinerary_list[length(min_itinerary_list)+1]<-destination_sequence
      min_destination_sequence=destination_sequence
    }
    destination_sequence <- destination_sequence[-length(destination_sequence)]
    
  }
  destination_sequence <- destination_sequence[-length(destination_sequence)]
}


min_price
min_destination_sequence

min_price_list
min_itinerary_list
