#include <R.h>
#include <Rinternals.h>
#include <vector>

namespace {

inline void validate_input(SEXP x) {
  if (!Rf_isMatrix(x)) {
    Rf_error("x must be a matrix with 4 columns");
  }

  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_length(dim) != 2 || INTEGER(dim)[1] != 4) {
    Rf_error("x must be a matrix with 4 columns");
  }

  if (TYPEOF(x) != INTSXP && TYPEOF(x) != REALSXP) {
    Rf_error("x must be an integer or numeric matrix");
  }
}

template <typename T>
SEXP filter_overlap_impl(SEXP x) {
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  const int nrows = INTEGER(dim)[0];
  const int ncols = INTEGER(dim)[1];
  T* data = reinterpret_cast<T*>(DATAPTR(x));

  if (nrows == 0) {
    return Rf_allocMatrix(TYPEOF(x), 0, ncols);
  }

  std::vector<int> keep(nrows, 1);

  for (int i = nrows - 1; i >= 1; --i) {
    const T s = data[i + nrows];
    const T e = data[i + (3 * nrows)];

    bool overlaps = false;
    for (int j = 0; j < i; ++j) {
      if (!keep[j]) {
        continue;
      }

      const T ps = data[j + nrows];
      const T pe = data[j + (3 * nrows)];
      if ((ps <= s && pe >= s) || (ps <= e && pe >= e)) {
        overlaps = true;
        break;
      }
    }

    if (overlaps) {
      keep[i] = 0;
    }
  }

  int kept = 0;
  for (int i = 0; i < nrows; ++i) {
    kept += keep[i];
  }

  SEXP out = PROTECT(Rf_allocMatrix(TYPEOF(x), kept, ncols));
  T* out_data = reinterpret_cast<T*>(DATAPTR(out));
  int row = 0;

  for (int i = 0; i < nrows; ++i) {
    if (!keep[i]) {
      continue;
    }

    for (int j = 0; j < ncols; ++j) {
      out_data[row + (j * kept)] = data[i + (j * nrows)];
    }
    ++row;
  }

  UNPROTECT(1);
  return out;
}

}  // namespace

extern "C" SEXP _mgsub_filter_overlap_cpp(SEXP x) {
  validate_input(x);

  switch (TYPEOF(x)) {
    case INTSXP:
      return filter_overlap_impl<int>(x);
    case REALSXP:
      return filter_overlap_impl<double>(x);
    default:
      Rf_error("x must be an integer or numeric matrix");
  }

  return R_NilValue;
}
