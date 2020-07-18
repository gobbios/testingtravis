#' Simulate simple FACS data
#'
#' @param n_aus numeric, number of action units (default is \code{5}). Can't be
#'        larger than 26.
#' @param n_events numeric, number of events (default is \code{200})
#' @param n_contexts numeric, number of contexts (default is \code{2}). Must be
#'        not larger than \code{n_aus}.
#' @param n_subjects numeric, number of subjects (default is \code{10})
#'
#' @return a \code{data.frame} with one row per event and one column for each
#'         AU, and columns for context, subject code and duration.
#' @export
#' @author Christof Neumann
#' @importFrom stats rpois runif
#' 
#' @details The function generates data such that a given AU will appear most
#'          often in one context and rarely in other contexts.
#'          
#'          Subjects become relevant only for the durations of events, i.e. some
#'          subjects tend to have longer events while others have shorter 
#'          events (which is independent of context).
#'          
#'          It is possible that the results contain events without any AU 
#'          present.
#'
#' @examples
#' xdata <- simulate_facs(n_aus = 5,
#'                        n_events = 200,
#'                        n_contexts = 1,
#'                        n_subjects = 3)
#' head(xdata)
#' boxplot(xdata$duration ~ xdata$subject)

simulate_facs <- function(n_aus = 5,
                          n_events = 200,
                          n_contexts = 2,
                          n_subjects = 10) {
  # some checks
  # need at least as many au's as contexts
  if (n_contexts > n_aus) stop("n_contexts can't be larger than n_aus")
  
  # label AUs
  if (n_aus > 26) stop("max n_aus is 26")
  aus <- LETTERS[1:n_aus]
  
  # label contexts
  ctxt <- paste0("ctxt_", 1:n_contexts)
  
  # spread AU's over contexts
  # make sure that each AU is assigned to exactly one context
  if (n_contexts == n_aus) {
    au_assign <- 1:n_contexts 
    } else {
      au_assign <- rep(1, n_aus)
      while (length(unique(au_assign)) < n_contexts) {
        au_assign <- sort(sample(1:n_contexts, size = n_aus, replace = TRUE))
      }
    }
  
  
  # results backbone
  out <- matrix(nrow = n_events, ncol = n_aus + 3, data = 0)
  colnames(out) <- c(aus, "subject", "context", "duration")
  
  out[, "subject"] <- sample(1:n_subjects, size = n_events, replace = TRUE)
  out[, "context"] <- sample(1:n_contexts, size = n_events, replace = TRUE)
  # durations are 'individualised', i.e. some individuals tend to have longer
  # events, others have shorter
  id_intercepts <- runif(n = n_subjects, min = 2, max = 5)
  id_intercepts <- id_intercepts[out[, "subject"]]
  out[, "duration"] <- rpois(n = n_events, lambda = id_intercepts) + 1
  
  # fill AUs
  sel <- c(1, 0)
  for (i in seq_len(n_events)) {
    # probabilities for AUs depending on context
    probs <- rep(0.1, n_aus)
    probs[au_assign == out[i, "context"]] <- 0.7
    # sample and store in 'out'
    out[i, 1:n_aus] <- sapply(probs, function(X)sample(sel, 1, prob = c(X, 1 - X)))
  }
  
  # re-format output
  data.frame(out[, 1:n_aus],
             context = ctxt[out[, "context"]],
             subject = paste0("id_", sprintf("%03.f", out[, "subject"])),
             duration = out[, "duration"],
             stringsAsFactors = FALSE)
}

