#' Take vector of elements and calculate probabilities of elements and combinations occurring
#'
#' Underlying code is C++ based
#'
#'
#' @param elements list with vectors for all elements observed together at each event
#' @param maxlen maximum size of combinations to be considered
#'
#' @return Function returns a dataframe with observed probabilities for each combination in the dataset
#' @author Christof Neumann
#' @export
#'
#' @examples
#' # xdata <- sapply(1:100, function(x)sort(sample(letters[1:12], size = sample(1:5, 1))))
#' # res <- create_rule_set2(elements = xdata, maxlen = 3)
#' # head(res, 10)

create_rule_set2 <- function(elements, maxlen) {
  xtab <- table(unlist(lapply(elements, comb_names, comb = maxlen)))
  xtab2 <- as.integer(xtab)
  res <- data.frame(combination = names(xtab),
                    observed.probability = xtab2 / length(elements),
                    count = xtab2,
                    stringsAsFactors = FALSE)
  res$combination.size <- unlist(lapply(strsplit(res$combination, "_"),
                                        length))
  res
}
