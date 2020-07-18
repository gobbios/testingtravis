#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
uint64_t choosecpp(uint64_t n, uint64_t k) {
  // https://stackoverflow.com/questions/60323747/how-to-create-a-combination-of-k-elements-between-n-in-rcpp
  if(k == 0) return 1;
  return (n * choosecpp(n - 1, k - 1)) / k;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix comb_index(int n_units, int comb) {
  // number of combinations and combination indices matrix
  // https://stackoverflow.com/questions/60323747/how-to-create-a-combination-of-k-elements-between-n-in-rcpp
  uint64_t n_combos = choosecpp(n_units, comb);
  std::string bitmask(comb, 1);
  bitmask.resize(n_units, 0);

  IntegerMatrix index_mat(n_combos, comb);
  uint64_t row_position = 0;
  do {
    uint64_t col_position = 0;
    for (int i = 0; i < n_units; ++i)  {
      if (bitmask[i]) {
        index_mat(row_position, col_position) = i; //+1
        col_position++;
      }
    }
    row_position++;
  } while (std::prev_permutation(bitmask.begin(), bitmask.end()));

  return index_mat;
}

// [[Rcpp::export]]
Rcpp::CharacterVector comb_names(CharacterVector elements, int comb) {
  int tot = 0;
  for (int i = 0; i < comb; ++i)  {
    tot = tot + choosecpp(elements.size(), i + 1);
  }

  List imats;
  for (int i = 0; i < comb; ++i)  {
    imats.push_back(comb_index(elements.size(), i + 1));
  }

  // navigation for index list (first column for list item, second for row within list item)
  IntegerMatrix nav_mat(tot, 2);

  int currow = 0;
  int xtimes = 0;
  for (int i = 0; i < comb; ++i)  {
    xtimes = choosecpp(elements.size(), i + 1);
    for (int k = 0; k < xtimes; ++k)  {
      nav_mat(currow, 0) = i;
      nav_mat(currow, 1) = k;
      currow = currow + 1;
    }
  }

  CharacterVector comb_names(tot);
  String temp_name = "";
  String temp_fill = "_";
  // int listindex;
  for (int i = 0; i < tot; ++i)  {
    // listindex = nav_mat(i, 0);
    IntegerMatrix tempmat = imats(nav_mat(i, 0));
    temp_name = elements(tempmat(nav_mat(i, 1), 0));
    for (int j = 1; j < tempmat.ncol(); j++) {
      temp_name +=  temp_fill + elements(tempmat(nav_mat(i, 1), j)) ;
    }
    comb_names[i] = temp_name;
    temp_name = "";
  }

  return comb_names;
}

//' Turn matrix into list
//'
//' @param inmat numeric or integer matrix, 0/1 matrix. Must have column names!
//' @return a list where each item represents a row in \code{inmat} and is a
//'         character vector with the column names for which a row value is 1
//' @details The function will fail if the input matrix does not have column
//'          names or if it is a not a \code{matrix}, but for example a
//'          \code{data.frame}.
//' @author Christof Neumann
//' @export
// [[Rcpp::export]]
Rcpp::List mat2list(NumericMatrix inmat) {
  // get column names
  SEXP s = inmat.attr("dimnames");
  List dn(s);
  CharacterVector cn = dn[1];
  // number of unit per event
  NumericVector lengthval = rowSums(inmat);
  // internal counter
  int counter = 0;
  // list for output
  List outnames(inmat.nrow());
  // loop through rows
  for (int i = 0; i < inmat.nrow(); ++i) {
    // empty names vector
    CharacterVector tempnames(lengthval(i));
    // loop through columns
    for (int k = 0; k < inmat.ncol(); ++k) {
      if (inmat(i, k) == 1) {
        tempnames(counter) = cn(k);
        counter = counter + 1;
      }
    }
    // add to output list
    outnames(i) = tempnames;
    // reset counter
    counter = 0;
  }
  return outnames;
}
