#' <Add Title>
#'
#' <Add Description>
#' @import Rcpp
#' @importFrom shiny restoreInput
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#' @importFrom Rcpp evalCpp
#' @useDynLib arbitree
#' @name arbitree
#'
#' @export
arbitreeInput <- function(inputId, reducedDimensions) {
  reactR::createReactShinyInput(
    inputId,
    "arbitree",
    htmltools::htmlDependency(
      name = "arbitree-input",
      version = "1.0.0",
      src = "www/arbitree/arbitree",
      package = "arbitree",
      script = "arbitree.js"
    ),
    jsonlite::toJSON(reducedDimensions),
    list(),
    htmltools::tags$span
  )
}

#' <Add Title>
#'
#' <Add Description>
#'
#' @export
updateArbitreeInput <- function(session, inputId, value, configuration = NULL) {
  message <- list(value = value)
  if (!is.null(configuration)) message$configuration <- configuration
  session$sendInputMessage(inputId, message);
}