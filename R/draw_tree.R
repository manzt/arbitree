#' Choose cells interactively to subset a cds
#'
#' @param input CDS object to subset
#'
#' @return A subset CDS object. If return_list = FALSE, a list of cell names.
#' @export
#'
draw_tree <- function(input) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("arbitree demo"),
    arbitreeInput("textInput"),
    shiny::textOutput("textOutput"),
    shiny::actionButton("done", "Done")
  )
  
  server <- function(input, output, session) {
    # output$textOutput <- shiny::renderText({
    #   sprintf("You entered: %s", input$textInput)
    # })
    
    shiny::observeEvent(input$done, {
      shiny::stopApp(input$textInput)
    })
  }
  sel <- suppressMessages(shiny::runApp(shiny::shinyApp(ui, server)))
  jsonlite::fromJSON(sel)
}
