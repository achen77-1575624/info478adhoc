# load your libraries
library("shiny")
library("shinyWidgets")
library("rsconnect")

# read in your files (make sure to double check path)
source("app_ui.R")
source("app_server.R")

# Create your shiny app by defining the UI and server
shinyApp(ui = ui, server = server)
