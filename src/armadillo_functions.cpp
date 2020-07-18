// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

//' Randomize entries in matrix while keeping row sums fixed
//'
//' This is a utility function and as such probably not of much interest for a
//' typical user.
//'
//' @param inmat numeric or integer matrix, 0/1 matrix
//' @return a randomized matrix with the same dimensions as \code{inmat}, where
//' the sum of values within a row remains constant
//' @details The returned matrix will not have column and row names even if the
//'          input matrix had them.
//' @author Christof Neumann
//' @examples
//' xdata <- matrix(ncol = 8, nrow = 5)
//' xdata[] <- sample(c(0, 1), length(xdata), TRUE)
//' res <- testingtravis:::shuffle_rowwise(xdata)
//' # row sums are constant
//' rowSums(xdata) == rowSums(res)
//' # column sums are not (although some may be, by chance)
//' colSums(xdata) == colSums(res)
//' @useDynLib testingtravis
// [[Rcpp::export]]
arma::umat shuffle_rowwise(arma::umat inmat) {
  inmat = arma::shuffle(inmat, 1);
  return inmat;
}

//' Randomize entries in matrix while keeping row and column sums fixed
//'
//' This is a utility function and as such probably not of much interest for a
//' typical user.
//'
//' @param inmat numeric or integer matrix, 0/1 matrix
//' @param swaps numeric, the number of (attempted/trial) swaps to perform
//'        (default is \code{1})
//' @param checkerboards_only logical, should the random selection of trial
//'        matrices to be swapped be constrained to checkerboards (default is
//'        \code{TRUE})
//' @return a randomized matrix with the same dimensions as \code{inmat}, where
//' row and column sums are constant
//' @details The algorithm starts by selecting a random submatrix with 2 rows
//'          and 2 columns (trial matrix). An actual swap is performed if this
//'          selected submatrix is a checkerboard. If \code{checkerboards_only =
//'          TRUE}, then the random selection is constrained to existing
//'          checkerboards. In other words, if \code{checkerboards_only = FALSE}
//'          a lot of swaps will not result in an actual swap and hence the
//'          final matrix will be 'less' randomized.
//'
//'          There is no accepted value to set \code{swaps}. One rule of thumb
//'          is to set it to double the number of 1's in the matrix (i.e.
//'          \code{sum(inmat) * 2}).
//'
//'          The returned matrix will not have column and row names even if the
//'          input matrix had them.
//' @author Christof Neumann
//' @examples
//' xdata <- matrix(ncol = 8, nrow = 5)
//' xdata[] <- sample(c(0, 1), length(xdata), TRUE)
//' res <- testingtravis:::shuffle_checkerboard(xdata, swaps = sum(xdata) * 2)
//' # row sums are constant
//' rowSums(xdata) == rowSums(res)
//' # column sums are constant too
//' colSums(xdata) == colSums(res)
// [[Rcpp::export]]
arma::umat shuffle_checkerboard(IntegerMatrix inmat, int swaps = 1, bool checkerboards_only = true) {
  // clone input to arma
  arma::umat inputdata = Rcpp::as<arma::umat>(inmat);
  // row and column indices for sampling
  IntegerVector indices_rows = Rcpp::seq(0, inmat.nrow() - 1);
  IntegerVector indices_cols = Rcpp::seq(0, inmat.ncol() - 1);
  arma::uvec ir = Rcpp::as<arma::uvec>(indices_rows);
  arma::uvec ic = Rcpp::as<arma::uvec>(indices_cols);

  int swap_count = 0;
  int tot_count = 0;

  if (checkerboards_only == true) {
    while (swap_count < swaps) {
      // select two rows and columns
      arma::uvec sel_rows = Rcpp::RcppArmadillo::sample(ir, 2, false);
      arma::uvec sel_cols = Rcpp::RcppArmadillo::sample(ic, 2, false);
      // extract submatrix
      arma::umat submat = inputdata(sel_rows, sel_cols);
      // check whether submat is checkerboard
      if (accu(submat) == 2 && ((trace(submat) == 2) | (trace(submat) == 0))) {
        inputdata(sel_rows, sel_cols) = inputdata(sel_rows, reverse(sel_cols));
        swap_count = swap_count + 1;
      }
      tot_count = tot_count + 1;
    }
  }

  if (checkerboards_only == false) {
    while (tot_count < swaps) {
      // select two rows and columns
      arma::uvec sel_rows = Rcpp::RcppArmadillo::sample(ir, 2, false);
      arma::uvec sel_cols = Rcpp::RcppArmadillo::sample(ic, 2, false);
      // extract submatrix
      arma::umat submat = inputdata(sel_rows, sel_cols);
      // check whether submat is checkerboard
      if (accu(submat) == 2 && ((trace(submat) == 2) | (trace(submat) == 0))) {
        inputdata(sel_rows, sel_cols) = inputdata(sel_rows, reverse(sel_cols));
        swap_count = swap_count + 1;
      }
      tot_count = tot_count + 1;
    }
  }

  return inputdata;
}

//' Simple ratio index
//'
//' This is a utility function and as such probably not of much interest for a
//' typical user.
//'
//' @param inmat integer matrix, rows represent observations/events and columns
//'        individuals/units
//' @return a square matrix with simple ratio index for pairs of individuals /
//'         units
//' @author Christof Neumann
// [[Rcpp::export]]
arma::mat sri_mat(IntegerMatrix inmat) {
  // clone input to arma
  arma::umat inputdata = Rcpp::as<arma::umat>(inmat);
  //arma::umat outmat(inmat.ncol(), inmat.ncol(), 0);
  arma::mat outmat = arma::zeros(inmat.ncol(), inmat.ncol());
  arma::mat tempmat1 = arma::zeros(1, 1);
  arma::mat tempmat2 = arma::zeros(1, 1);
  for (int i = 0; i < inmat.ncol(); i++) {
    for (int j = i + 1; j < inmat.ncol(); j++) {
      tempmat1(0, 0) = accu(inputdata.col(i) + inputdata.col(j) == 1);
      tempmat2(0, 0) = accu(inputdata.col(i) + inputdata.col(j) == 2);
      outmat(i, j) = tempmat2(0, 0) / (tempmat2(0, 0) + tempmat1(0, 0) * 1.0);
      outmat(j, i) = outmat(i, j);
    }
  }
  return outmat;
}

