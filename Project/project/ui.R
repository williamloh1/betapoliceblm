library(shiny)  ## Load shiny

statecodes <- read.csv("~/Documents/GitHub/teambeta/Project/datasets/state codes.csv")

shinyUI(fluidPage( ## Flexible layout function 
  titlePanel("COVID-19 Search Trends"), ## Setting the title
  sidebarLayout( ## Choosing layout with inputs on side and 
    ## outputs displayed in the main body
    sidebarPanel( #Things in this function specify the sidebar
      selectInput("dataset", "Choose a state:",
                  choices = statecodes$state, inputId = "stateterm"),
      textInput(inputId = "searchterm", 
                label = "Search Term:",
                value=""
      )), ## End of sidebar
    mainPanel( ## Arguments for main section (output)
      plotOutput("searchGraph")
    ) # Close main panel
  ) #
))