getDistances <- function(x){
  # A function to get distances from point A to B that can be used easily with apply 
  # Called from df() to find the longest segment
  
  # These columns correspond to  lon end, lat end, long start, lat start
  gdist(x[1],x[2],x[3],x[4], units='m')
  
  # An alternative geodesic distance function I tried -- same results (requires a different package)
  # geosphere::distGeo(c(x[1], x[2]),c(x[3],x[4]))
}


allSegments <- function(dataset, shipname){
  # Extract all segments of a chosen vessel (shipname), and organize into a dataframe that allows
  # easy assessment of distances, with the first row containing the location of last segment (e.g. 100)
  # and one-before-last (e.g. 99), the next row has 99 and 98, etc. until the last row has 1 and 1.
  # This is not space-efficient but I thought it was the simplest way to get to a row-wise use of gdist. 
  # I couldn't find a way to you gdist.total or distGeo on a segment list except a for loop, but I didn't 
  # rigorously check if that was indeed faster.
  
  # Select only the lon, lat, and times for the selected vessel and organize from most to least recent
  segments <- dataset %>% filter(SHIPNAME==shipname) %>%
    select(LON, LAT, DATETIME) %>%
    mutate(DATETIME = as.POSIXct(DATETIME, format = '%Y-%m-%dT%H:%M:%OS' , tz = 'UTC')) %>%
    arrange(desc(DATETIME))
  
  # Build a dataframe where each row contains two points - current and past - and their timestamps
  segments$prev_lon <- c(segments$LON[-1], tail(segments$LON,1))
  segments$prev_lat <- c(segments$LAT[-1], tail(segments$LAT,1))
  segments$prev_datetime <- structure(c(segments$DATETIME[-1],tail(segments$DATETIME,1)), tzone='UTC')
  
  return (segments)
}