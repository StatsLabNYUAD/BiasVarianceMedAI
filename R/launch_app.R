#' Launch Bias-Variance Analysis App
#'
#' Alternative function name for launching the Shiny application
#'
#' @param ... Arguments passed to run_app()
#' @return Shiny app object
#' @export
#'
#' @examples
#' if (interactive()) {
#'   launch_bias_variance_app()
#' }
launch_bias_variance_app <- function(...) {
  run_app(...)
}

