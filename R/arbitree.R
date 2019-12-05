demo = tibble::tibble(x=rnorm(100), y=rnorm(100), cluster=sample(c("A", "B", "C"), 100, replace=TRUE))

#' <Add Title>
#'
#' <Add Description>
#'
#' @importFrom shiny restoreInput
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags
#'
#' @export
arbitreeInput <- function(inputId, default = demo) {
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
    jsonlite::toJSON(default),
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