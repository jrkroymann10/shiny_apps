# installing and loading shiny package
install.packages("shiny")
library("shiny")

# sourcing ui and server files
source("ui.R")
source("server.R")

# creating shiny application
shinyApp(ui = ui, server = server)