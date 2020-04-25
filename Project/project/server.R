library(shiny) # call to shiny

shinyServer(function(input, output) { ## Funciton defenition
  ## server behavior is defined in here
  ## In this case, we are going to add a component to 
  ## the output.
  output$textDisplay <- renderText({ # We need to creat the text here
    # Note that `textDisplay` is the same as in the UI
    # where it is included in the textoutput() call
    paste0("You said '", input$comment, # the ID specified in the UI
           ".' There are ", nchar(input$comment), 
           " characters in this."
    ) #close paste0
  }) #Close renderText 
}) # Close function/shinyServer call