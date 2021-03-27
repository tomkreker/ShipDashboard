# Specify libraries to install on shinyapps.io
library(shiny)
library(shiny.semantic)
library(Imap)              # Used for finding the distance between subsequent segments
library(dplyr)             # Manipuate data
library(leaflet)           # Builds the map 

# Explicit command to load all libraries
pkgload::load_all(".")

# Wrapper function for the app
shipApp <-function(...){
  
  ui <- semanticPage(
    title = 'Vessel Movement Dashboard',
    
    # container for the top part
    div(class='ui container',
        div(class='ui two column grid',
            div(class='column', id='dropdowns',
                div(dropdownsUI('dropdowns', dataset)) # dropdowns
            ),
            div(class='column', id='note',
                div(htmlOutput('note'))                 # notes
            )
        )
    ),
    
    # A dividing line
    div(class = 'ui divider'),
    
    # The map
    div(class = 'ui raised segment',
        div(leafletOutput('map'))
    )
    
  ) # end of ui
  
  server <- function(input, output, session) {
    
    # Call dropdown module and save user input as result
    dropdowns <- dropdownsServer('dropdowns')
    
    # Define a function to get distances from point A to B that can be used easily with apply 
    # Called from df() to find the longest segment
    getDistances <- function(x){
      
      # These columns correspond to  lon 1, lon 2, lat 1, lat 2
      gdist(x[1],x[2],x[3],x[4], units='m')

      # An alternative geodesic distance function I tried -- same results (requires a different package)
      # geosphere::distGeo(c(x[1], x[2]),c(x[3],x[4]))
    }

    # Extract the longest distance and pass it as a dataframe to note and map outputs
    # format: Endpoint Lat, Endpoint Lon, Endpoint Datetime, and similarly for the start point of the segment
    df <- reactive({

      # Validate dropdown values were chosen
      validate(need(req(dropdowns$user_vessel())!='Choose Vessel', message = FALSE))

      # Select only the lon, lat, and times for the selected vessel and organize from most to least recent
      test_ships <- dataset %>% filter(SHIPNAME==dropdowns$user_vessel()) %>%
        select(LON, LAT, DATETIME) %>%
        mutate(DATETIME = as.POSIXct(DATETIME, format = '%Y-%m-%dT%H:%M:%OS' , tz = 'UTC')) %>%
        arrange(desc(DATETIME))

      # Build a dataframe where each row contains two points - current and past - and their timestamps
      test_ships$prev_lon <- c(test_ships$LON[-1], tail(test_ships$LON,1))
      test_ships$prev_lat <- c(test_ships$LAT[-1], tail(test_ships$LAT,1))
      test_ships$prev_datetime <- structure(c(test_ships$DATETIME[-1],tail(test_ships$DATETIME,1)), tzone='UTC')

      # Get the distances between each pair; which.max will later choose the earlier one (recent) if there is a tie
      distances <- apply(test_ships %>% select(-DATETIME, -prev_datetime),1,getDistances)

      # Return a single-row dataframe at the max distance with the two points, times, and distance between them
      return(test_ships[which.max(distances),] %>% mutate(distance=max(distances)))


    })

    # Print the note
    output$note <- renderText({

      # Only print note after the user selects a vessel type and vessel
      validate(
        need(req(dropdowns$user_vessel())!='Choose Vessel',
             'Please choose a vessel to see information about the longest segment.')
      )

      # Create the HTML string to print as the note
      paste(strong('Longest Segment Distance:'),round(df()$distance,2),'meters', '<br/>',
            'Segment traveled between', strftime(df()$prev_datetime, format='%B %d, %Y', tz='utc'), 'at',
            strftime(df()$prev_datetime, format='%H:%M', tz='utc'), 'and',
            strftime(df()$DATETIME, format='%B %d, %Y', tz='utc'), 'at', strftime(df()$DATETIME, format='%H:%M', tz='utc')
      )

    })

    # Create icon style for the map
    shipIcon <-makeIcon(iconRetinaUrl = 'www/ship.png', 24, 24)

    # Create map object
    output$map <- renderLeaflet({

      # Validate a ship was chosesn
      validate(need(req(dropdowns$user_vessel())!='Choose Vessel','Please choose a vessel to view the map.'))

      # Build map object
      leaflet() %>%
        addTiles() %>% 
        setView(lng = df()$LON, lat=df()$LAT, zoom=9) %>%  # this could be set dynamically based on distance
        addMarkers(lng=df()$LON, lat=df()$LAT, label='End of longest segment', icon = shipIcon) %>% # start of segment
        addMarkers(lng=df()$prev_lon, lat=df()$prev_lat, label='Start of longest segment', icon = shipIcon) %>% # end segment

        # draw a line between them
        addPolylines(lng = c(df()$LON, df()$prev_lon), lat = c(df()$LAT, df()$prev_lat), weight=2, color='black',dashArray = '4')

    })
  }
  
  shinyApp(ui, server, ...)
}

shipApp()