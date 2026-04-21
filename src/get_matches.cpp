#include <R.h>
#include <Rinternals.h>
#include <vector>

namespace {

SEXP match_length_symbol() {
  static SEXP sym = R_NilValue;
  if (sym == R_NilValue) {
    sym = Rf_install("match.length");
  }
  return sym;
}

SEXP build_gregexpr_call(SEXP pattern_i, SEXP string, SEXP dots) {
  SEXP call = PROTECT(Rf_lcons(Rf_install("gregexpr"), R_NilValue));
  SEXP tail = call;

  SETCDR(tail, Rf_cons(pattern_i, R_NilValue));
  tail = CDR(tail);

  SETCDR(tail, Rf_cons(string, R_NilValue));
  tail = CDR(tail);

  if (dots != R_NilValue) {
    if (!Rf_isNewList(dots)) {
      UNPROTECT(1);
      Rf_error("dots must be a list");
    }

    SEXP names = Rf_getAttrib(dots, R_NamesSymbol);
    const R_xlen_t n_dots = XLENGTH(dots);
    for (R_xlen_t idx = 0; idx < n_dots; ++idx) {
      SEXP node = PROTECT(Rf_cons(VECTOR_ELT(dots, idx), R_NilValue));
      if (names != R_NilValue && idx < XLENGTH(names)) {
        const char* name = CHAR(STRING_ELT(names, idx));
        if (name[0] != '\0') {
          SET_TAG(node, Rf_install(name));
        }
      }
      SETCDR(tail, node);
      tail = node;
      UNPROTECT(1);
    }
  }

  UNPROTECT(1);
  return call;
}

inline int scalar_index(SEXP i) {
  if (Rf_length(i) != 1) {
    Rf_error("i must have length 1");
  }

  switch (TYPEOF(i)) {
    case INTSXP:
      if (INTEGER(i)[0] == NA_INTEGER) {
        Rf_error("i must not be NA");
      }
      return INTEGER(i)[0];
    case REALSXP:
      if (ISNA(REAL(i)[0]) || ISNAN(REAL(i)[0])) {
        Rf_error("i must not be NA");
      }
      return static_cast<int>(REAL(i)[0]);
    default:
      Rf_error("i must be numeric");
  }

  return 0;
}

}  // namespace

extern "C" SEXP _mgsub_get_matches_cpp(SEXP string, SEXP pattern, SEXP i,
                                       SEXP dots) {
  if (TYPEOF(pattern) != STRSXP || Rf_length(pattern) < 1) {
    Rf_error("pattern must be a character vector");
  }

  const int idx = scalar_index(i);
  if (idx < 1 || idx > Rf_length(pattern)) {
    Rf_error("i is out of bounds");
  }

  SEXP pattern_i = PROTECT(Rf_ScalarString(STRING_ELT(pattern, idx - 1)));
  SEXP call = PROTECT(build_gregexpr_call(pattern_i, string, dots));
  SEXP tmp = PROTECT(Rf_eval(call, R_BaseEnv));

  if (!Rf_isNewList(tmp) || Rf_length(tmp) < 1) {
    UNPROTECT(3);
    Rf_error("gregexpr did not return a list");
  }

  SEXP start = PROTECT(Rf_coerceVector(VECTOR_ELT(tmp, 0), INTSXP));

  SEXP length = Rf_getAttrib(VECTOR_ELT(tmp, 0), match_length_symbol());
  if (length == R_NilValue) {
    UNPROTECT(4);
    Rf_error("gregexpr result did not include match.length");
  }
  length = PROTECT(Rf_coerceVector(length, INTSXP));

  const R_xlen_t n = XLENGTH(start);
  SEXP out = PROTECT(Rf_allocMatrix(INTSXP, static_cast<int>(n), 4));
  int* out_data = INTEGER(out);
  const int* start_data = INTEGER(start);
  const int* length_data = INTEGER(length);

  for (R_xlen_t row = 0; row < n; ++row) {
    const int start_i = start_data[row];
    const int length_i = length_data[row];
    out_data[row] = idx;
    out_data[row + n] = start_i;
    out_data[row + (2 * n)] = length_i;
    out_data[row + (3 * n)] = start_i + length_i - 1;
  }

  UNPROTECT(6);
  return out;
}
