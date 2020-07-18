
#' doing a histogram
#'
#' @param n numeric
#'
#' @return a plot
#' @export
#' @importFrom stats rnorm
#' @importFrom graphics hist
#'
#' @examples
#' do_a_hist(100)
do_a_hist <- function(n) {
  hist(rnorm(n))
}
