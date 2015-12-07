library(readr)
library(magrittr)
library(leaflet)
library(dplyr)
library(magrittr)

SAMPLE <- FALSE
set.seed(22)

# Load data
gifts <- read_csv(file = "data/gifts.csv")
sample.sub <- read_csv(file = "data/sample_submission.csv")
gifts.matrix <- as.matrix(gifts)

# Create trips
remaining.gifts <- gifts.matrix
submission <- matrix(nrow = 0, ncol = 2)
trip.id <- 0

while(nrow(remaining.gifts) > 0){
  
  trip.ids <- CreateTrip(remaining.gifts)
  nremaining.gifts <- remaining.gifts[!(remaining.gifts[,1] %in% trip.ids), ]
  submission <- rbind(submission, 
                      matrix(data = c(trip.ids, 
                                      rep(trip.id, length(trip.ids))),
                             ncol = 2, byrow = FALSE))
  trip.id <- trip.id + 1
  cat("Countdown: ", nrow(remaining.gifts) / nrow(gifts.matrix) * 100, "%\n")
  
}

# Submission evaluation
sub <- submission %>% left_join(gifts, by = c("GiftId")) %>% as.matrix()
print(system.time({  
  cost <- 0.0
  cost <- FastSumissionEval(sub)
  cat(cost, fill = T)
}))



if(SAMPLE){
  # Submission evaluation example
  sub <- sample.sub %>% left_join(gifts, by = c("GiftId")) %>% as.matrix()
  print(system.time({  
    dist=0.0
    dist = FastSumissionEval(sub)
    cat(dist, fill = T)
  }))
  
  # Create trip example
  trip.ids <- CreateTrip(gifts.matrix)
  trip <- gifts.matrix[gifts.matrix[, 1] %in% trip.ids, ]
  
  # Plot all gifts in the trip
  m <- leaflet() %>%
    addTiles()  # Add default OpenStreetMap map tiles
  
  for(i in 1:nrow(trip)){
    m %<>% addMarkers(lng=trip[i, 3], lat=trip[i, 2])
  }
}
