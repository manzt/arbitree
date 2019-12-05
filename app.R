library(shiny)
library(arbitree)

ui <- fluidPage(
  titlePanel("reactR Input Example"),
  arbitreeInput("textInput"),
  textOutput("textOutput")
)

server <- function(input, output, session) {
  output$textOutput <- renderText({
    sprintf("You entered: %s", input$textInput)
  })
}

shinyApp(ui, server)