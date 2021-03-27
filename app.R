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

    # Extract the longest distance and pass it as a dataframe to note and map outputs
    # format: Endpoint Lat, Endpoint Lon, Endpoint Datetime, and similarly for the start point of the segment
    df <- reactive({

      # Validate dropdown values were chosen
      validate(need(req(dropdowns$user_vessel())!='Choose Vessel', message = FALSE))
      
      # Build a dataframe with all segments where each row contains an end and start of segment
      segments <- allSegments(dataset, shipname = dropdowns$user_vessel())

      # Get the distances between each pair; which.max will later choose the earlier one (recent) if there is a tie
      distances <- apply(segments %>% select(-DATETIME, -prev_datetime), 1, getDistances)

      # Return a single-row dataframe at the max distance with the two points, times, and distance between them
      return(segments[which.max(distances),] %>% mutate(distance=max(distances)))

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
  } # end of server
  
  shinyApp(ui, server, ...)
}

shipApp()