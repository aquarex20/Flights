
library(rflights)
country_origin="Canada"
city_origin="Montreal"
city_destination="Milan"
time_frame=30 #in the next 30 days for example 

# Find your City or Country's location ID
cat("=== Finding", city_origin, " location ID ===\n")
origin_locs <- find_location(city, location_types = c("CITY"))

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

min_price=1000

flights <- search_flights(
  fly_from       = new_origin_id,
  departure_from = Sys.Date(),
  departure_to   = Sys.Date() + time_frame,
)
destination_sequence=list()
min_destination_sequence=list()

for (i in seq_len(nrow(flights$itineraries))) {
  dest  <- flights$itineraries$destination$station$city$name[i]
  dest_id  <- flights$itineraries$destination$station$id[i]
  if (is.na(dest)) next
  
  flights <- search_flights(
    fly_from       = new_origin_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame,
    fly_to = dest_id
  )
  #2 possibilities, either parkour them all, to find best fit compatible time-frame. 
  # or choose the cheapest and hope it's all finna be good.
  #we'll choose the cheapest for this example. 
  #find the minimum price from your origin to your destination because when you ask for destinations reachable from an origin it doesnt give you every possibility of getting there, just the possibilities. 
  price <- flights$itineraries$price$amount[i]
  total_price1=total_price+as.numeric(price)
  destination_sequence[length(destination_sequence)+1]<-dest
  
  if (total_price1>min_price){      destination_sequence <- destination_sequence[-length(destination_sequence)]
  
  next}
  if (dest==city_destination){
    min_price=total_price1
    min_destination_sequence=destination_sequence
  }
  
  flights <- search_flights(
    fly_from       = dest_id,
    departure_from = Sys.Date(),
    departure_to   = Sys.Date() + time_frame,
  )
  
  for (i in seq_len(nrow(flights$itineraries))) {
    dest  <- flights$itineraries$destination$station$city$name[i]
    dest_id2  <- flights$itineraries$destination$station$id[i]
    if (is.na(dest)) next
    
    price <- flights$itineraries$price$amount[i]
    total_price2=total_price1+as.numeric(price)
    destination_sequence[length(destination_sequence)+1]<-dest
    
    if (total_price2>min_price){      destination_sequence <- destination_sequence[-length(destination_sequence)]
    
    next}
    if (dest==city_destination){
      min_price=total_price2
      min_destination_sequence=destination_sequence
    }
    flights <- search_flights(
      fly_from       = dest_id2,
      departure_from = Sys.Date(),
      departure_to   = Sys.Date() + time_frame,
    )
    
    for (i in seq_len(nrow(flights$itineraries))) {
      dest  <- flights$itineraries$destination$station$city$name[i]
      dest_id3  <- flights$itineraries$destination$station$id[i]
      if (is.na(dest)) next
      
      price <- flights$itineraries$price$amount[i]
      print(dest)
      print(price)
      total_price3=total_price2+as.numeric(price)
      print(total_price3)
      
      destination_sequence[length(destination_sequence)+1]<-dest
      
      if (total_price3>min_price){
        destination_sequence <- destination_sequence[-length(destination_sequence)]
        
        next}
      if (dest==city_destination){
        min_price=total_price3
        min_destination_sequence=destination_sequence
      }
      flights <- search_flights(
        fly_from       = dest_id3,
        departure_from = Sys.Date(),
        departure_to   = Sys.Date() + time_frame,
      )
      
      for (i in seq_len(nrow(flights$itineraries))) {
        dest  <- flights$itineraries$destination$station$city$name[i]
        dest_id4  <- flights$itineraries$destination$station$id[i]
        if (is.na(dest)) next
        
        price <- flights$itineraries$price$amount[i]
        total_price4=total_price3+as.numeric(price)
        destination_sequence[length(destination_sequence)+1]<-dest
        
        if (total_price4>min_price){
          destination_sequence <- destination_sequence[-length(destination_sequence)]
          
          next}
        if (dest==city_destination){
          min_price=total_price4
          min_destination_sequence=destination_sequence
        }
        destination_sequence <- destination_sequence[-length(destination_sequence)]
      }
      destination_sequence <- destination_sequence[-length(destination_sequence)]
      
    }
    destination_sequence <- destination_sequence[-length(destination_sequence)]
    
  }
  destination_sequence <- destination_sequence[-length(destination_sequence)]
  
}


min_price
min_destination_sequence
