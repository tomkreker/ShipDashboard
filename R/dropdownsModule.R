# Module UI function
dropdownsUI <- function(id, dataset) {
  
  # Define a namespace function
  ns <- NS(id)
  
  # Create the two dropdowns. I use default selector values because it feels cleaner to me than auto-picking the
  # first or putting an empty one.
  tagList(
    selectInput(ns('user_type'), 'Select Vessel Type:',choices = c('Choose Vessel Type', unique(dataset$ship_type)), 
                selected = 'Choose Vessel Type'),
    selectInput(ns('user_vessel'), 'Select Vessel:', c('First Choose Vessel Type'='Choose Vessel'))
  )
}

# Module server function
dropdownsServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
      
    # Update the dataframe for all relevant options
    shipNames <- reactive({
      dataset %>% filter(ship_type==input$user_type) %>% distinct(SHIPNAME) %>% pull
    })
    
    # Update the values of the ship names in the dropdown based on the selected ship type
    observe({
      
      # When the user chooses a type
      if (req(input$user_type) != 'Choose Vessel Type'){
        
        # Ensure no freezes happen when user switches type after selecting a specific vessel 
        freezeReactiveValue(input, 'user_vessel')
        
        # Update options to include all ship names of ships of the selected type
        updateSelectInput(session, 'user_vessel', choices = c('Choose Vessel', shipNames()), selected = 'Choose Vessel') 
      }
      
      # This section can change a behavior in which a user selects a type, then a ship, then sets the type
      # to Choose Vessel Type again, and nothing happens. The user still sees the same ship list from the
      # previous type. I think that's okay but if desired, this would also reset the ship name list after
      # selecting type as Choose Vessel Type.
      
      # else if (req(input$user_type) == 'Choose Vessel Type'){
      #   # Ensure no freezes happen when user switches type after selecting a specific vessel 
      #   freezeReactiveValue(input, 'user_vessel')
      #   
      #   # Update options to include all ship names of ships of the selected type
      #   updateSelectInput(session, 'user_vessel', choices = c('First Choose Vessel Type'='Choose Vessel'))
      # }
    })
    
    # Return both user inputs to be used to produce the note and map
    list(
      user_type = reactive(input$user_type),
      user_vessel = reactive(input$user_vessel)
    )
      
  }) # end of moduleServer    
}