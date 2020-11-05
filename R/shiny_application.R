#' Shiny application
#'
#' This function allows to launch the shiny app.
#' @param host The IPv4 address that the application should listen on.
#' @param port The TCP port that the application should listen on.
#' @export
#' @examples shiny_application()
shiny_application <- function(host='0.0.0.0', port=3838) {
  appDir <- system.file("application", package = "PIPprofileR")
  shiny::runApp(appDir, host=host, port=port)
}
