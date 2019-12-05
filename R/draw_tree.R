#' Choose cells interactively to subset a cds
#'
#' @param input CDS object to subset
#'
#' @return A subset CDS object. If return_list = FALSE, a list of cell names.
#' @export
#'
draw_tree <- function(cds, reduction_method = c("UMAP", "tSNE", "PCA", "Aligned"), color_cells_by="cluster") {
  reduction_method <- match.arg(reduction_method)
  
  reduced_dims <- as.data.frame(reducedDims(cds)[[reduction_method]])
  names(reduced_dims)[1:2] <- c("x", "y")
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("arbitree demo"),
    arbitreeInput("data", reduced_dims),
    shiny::textOutput("textOutput"),
    shiny::actionButton("done", "Done")
  )
  
  server <- function(input, output, session) {
    # output$textOutput <- shiny::renderText({
    #   sprintf("You entered: %s", input$textInput)
    # })
    
    shiny::observeEvent(input$done, {
      shiny::stopApp(input$data)
    })
  }
  sel <- suppressMessages(shiny::runApp(shiny::shinyApp(ui, server)))
  jsonlite::fromJSON(sel)
}
