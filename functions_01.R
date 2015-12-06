CreateTrip <- function(remaining.gifts, max.retry = 3){
# remaining.gifts is a matrix with the next columns:
#   1: gift id
#   2: latitude
#   3: longitude
#   4: weight
# Returns a vector with a "pack" of gift ids for a trip
  
  # Initialize
  total.weight <- 0
  trip.gift.ids <- c()
  
  # Take a random first gift
  first.gift.index <- 
    runif(n = 1, min = 0, max = nrow(remaining.gifts)) %>% round()
  first.gift <- remaining.gifts[first.gift.index, ]
  remaining.gifts <- remaining.gifts[-first.gift.index, ]
  total.weight <- total.weight + first.gift[4]

  # Take gifts while total.weight is less than 1000
  while(total.weight < 1000 && max.retry > 0 && nrow(remaining.gifts) > 0){
    
    next.gift.id <- FastTakeNearestGift(remaining.gifts, as.matrix(first.gift))
    weight <- remaining.gifts[remaining.gifts[, 1] == next.gift.id, 4]
    
    if(total.weight + weight > 1000){
      max.retry <- max.retry - 1
      next()
    }
      
    total.weight <- total.weight + weight
    trip.gift.ids <- append(trip.gift.ids, next.gift.id)
    remaining.gifts <- remaining.gifts[remaining.gifts[, 1] != next.gift.id, ]
    
  }
  
  trip.gift.ids
  
}
