#' @export
ProdACM <- function() {
  appDir <- system.file("AppACM", "myapp", package = "ProdFacto")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ProdFacto`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
