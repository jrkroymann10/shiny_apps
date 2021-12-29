# installing and loading shiny package
install.packages("shiny")
library("shiny")

# sourcing ui and server files
source("code/ui.R")
source("code/server.R")

# creating shiny application
shinyApp(ui = ui, server = server)