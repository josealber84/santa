#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double FastAsRadians(double theta = 0){
  return theta * PI / 180;
}

// [[Rcpp::export]]
double FastHaversine(double lat1, double lng1, 
                     double lat2, double lng2){
  
  lat1 = FastAsRadians(lat1);
  lat2 = FastAsRadians(lat2);
  lng1 = FastAsRadians(lng1);
  lng2 = FastAsRadians(lng2);
  
  double lat = lat2 - lat1;
  double lng = lng2 - lng1;
  double d = pow(sin(lat / 2), 2) + cos(lat1) * cos(lat2) * pow(sin(lng / 2), 2);
  double h = 2 * 6371 * asin(sqrt(d)); // AVG_EARTH_RADIUS = 6371
  return h;
  
}

// [[Rcpp::export]]
double FastTripEval(NumericMatrix trip){
  // Each row in trip is a gift. A trip starts and stops in the North Pole.
  // Columns in trip:
  //  0: latitude
  //  1: longitude
  //  2: weight
  
  double cost = 0;
  double actual_latitude = 90; // North Pole
  double actual_longitude = 0; // Noth Pole
  double distance = 0;
  double weigth = 10; // Trineo
  
  // Calculate total weight
  for(int i = 0; i < trip.nrow(); i++){
    weigth = weigth + trip(i, 2);
  }
    
  // Calculate trip cost except last travel
  for(int i = 0; i < trip.nrow(); i++){
    
    // Calculate cost
    distance = FastHaversine(actual_latitude, actual_longitude, trip(i, 0), trip(i, 1));
    cost = cost + distance * weigth;
    
    // Refresh values
    actual_latitude  = trip(i, 0);
    actual_longitude = trip(i, 1);
    weigth  = weigth - trip(i, 2);
    
  }
  
  // Add last travel (back to North Pole)
  if(round(weigth) != 10){
    printf("ERROR: weight value should be 10, and it is %f\n", weigth);
  }
  distance = FastHaversine(actual_latitude, actual_longitude, 90, 0);
  cost = cost + distance * weigth;
  
  
  // Return
  return cost;
  
}

// [[Rcpp::export]]
double FastSumissionEval(NumericMatrix submission){
  // Each row in submission is a gift. A trip starts and stops in the North Pole.
  // Columns in submission:
  //  0: gift id
  //  1: trip id
  //  2: latitude
  //  3: longitude
  //  4: weight
  
  double cost = 0;
  
  // Get number of trips
  int n_trips = max(submission.column(1)) + 1;
  printf("%i trips detected\n", n_trips);
  
  for(int trip_id = 0; trip_id < n_trips; trip_id++){
    
    int nrows = 0;
    for(int row = 0; row < submission.nrow(); row++){
      if(round(submission(row, 1)) == trip_id){
        nrows++;
      }
    }
    
    NumericMatrix trip = NumericMatrix(nrows, 3);
    int trip_row = 0;
    for(int row = 0; row < submission.nrow(); row++){
      if(round(submission(row, 1)) == trip_id){
        for(int col = 2; col < submission.ncol(); col++){
          trip(trip_row, col-2) = submission(row, col);
        }
        trip_row++;
      }
    }
    
    cost = cost + FastTripEval(trip);
    
  }
  
  // Return
  printf("Cost = %f\n", cost);
  return cost;
  
}

// [[Rcpp::export]]
int FastTakeNearestGift(NumericMatrix remaining_gifts, 
                                  NumericMatrix gift){
  // Both gift and remaiining_gifts have the next columns:
  //  0: gift id
  //  1: latitude
  //  2: longitude
  //  3: weight
  // Returns the id of the nearest gift
  
  if(remaining_gifts.nrow() == 0){
    printf("No more remaining gifts! \n");
    return -1;
  }
  
  // Initialize min distance
  double min_distance = FastHaversine(remaining_gifts(0, 1), 
                                      remaining_gifts(0, 2),
                                      gift(0, 1), gift(0, 2));
  double distance = 0;
  
  // Get nearest gift
  int nearest_gift_id = -1;
  for(int row = 0; row < remaining_gifts.nrow(); row++){
    distance = FastHaversine(remaining_gifts(row, 1),  remaining_gifts(row, 2),
                             gift(0, 1), gift(0, 2));
    if(distance <= min_distance){
      min_distance = distance;
      nearest_gift_id = remaining_gifts(row, 0);
    }
  }
  
  // Return
  return nearest_gift_id;
  
}