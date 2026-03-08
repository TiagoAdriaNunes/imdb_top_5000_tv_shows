# Entry point - allows running from the project root with: shiny::runApp()
# Changes working directory to app/ so all relative paths resolve correctly
setwd(file.path(getwd(), "app"))

box::use(shiny[shinyApp])
options(box.path = getwd())

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
