#include <algorithm>
#include <numeric>
#include <vector>
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

namespace {

template <typename T>
const T* matrix_data(SEXP x);

template <>
const int* matrix_data<int>(SEXP x) {
  return INTEGER(x);
}

template <>
const double* matrix_data<double>(SEXP x) {
  return REAL(x);
}

template <typename T>
T* matrix_data_mutable(SEXP x);

template <>
int* matrix_data_mutable<int>(SEXP x) {
  return INTEGER(x);
}

template <>
double* matrix_data_mutable<double>(SEXP x) {
  return REAL(x);
}

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
SEXP resolve_matches_impl(SEXP x) {
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  const int nrows = INTEGER(dim)[0];
  const int ncols = INTEGER(dim)[1];
  const T* data = matrix_data<T>(x);

  if (nrows <= 1) {
    SEXP out = PROTECT(Rf_allocMatrix(TYPEOF(x), nrows, ncols));
    T* out_data = matrix_data_mutable<T>(out);
    for (int j = 0; j < ncols; ++j) {
      for (int i = 0; i < nrows; ++i) {
        out_data[i + (j * nrows)] = data[i + (j * nrows)];
      }
    }
    UNPROTECT(1);
    return out;
  }

  std::vector<int> order(static_cast<size_t>(nrows));
  std::iota(order.begin(), order.end(), 0);

  std::stable_sort(order.begin(), order.end(),
                   [&](int lhs, int rhs) {
                     return data[lhs + (2 * nrows)] > data[rhs + (2 * nrows)];
                   });

  std::vector<int> keep;
  keep.reserve(static_cast<size_t>(nrows));

  for (int idx : order) {
    const T s = data[idx + nrows];
    const T e = data[idx + (3 * nrows)];
    bool overlaps = false;

    for (int kept_idx : keep) {
      const T ps = data[kept_idx + nrows];
      const T pe = data[kept_idx + (3 * nrows)];
      if ((ps <= s && pe >= s) || (ps <= e && pe >= e)) {
        overlaps = true;
        break;
      }
    }

    if (!overlaps) {
      keep.push_back(idx);
    }
  }

  std::stable_sort(keep.begin(), keep.end(),
                   [&](int lhs, int rhs) {
                     return data[lhs + nrows] < data[rhs + nrows];
                   });

  const int kept = static_cast<int>(keep.size());
  SEXP out = PROTECT(Rf_allocMatrix(TYPEOF(x), kept, ncols));
  T* out_data = matrix_data_mutable<T>(out);

  for (int row = 0; row < kept; ++row) {
    const int src = keep[static_cast<size_t>(row)];
    for (int col = 0; col < ncols; ++col) {
      out_data[row + (col * kept)] = data[src + (col * nrows)];
    }
  }

  UNPROTECT(1);
  return out;
}

}  // namespace

extern "C" SEXP _mgsub_resolve_matches_cpp(SEXP x) {
  validate_input(x);

  switch (TYPEOF(x)) {
    case INTSXP:
      return resolve_matches_impl<int>(x);
    case REALSXP:
      return resolve_matches_impl<double>(x);
    default:
      Rf_error("x must be an integer or numeric matrix");
  }

  return R_NilValue;
}
