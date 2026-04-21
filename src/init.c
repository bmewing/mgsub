#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP _mgsub_filter_overlap_cpp(SEXP);
extern SEXP _mgsub_get_matches_cpp(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_mgsub_filter_overlap_cpp", (DL_FUNC) &_mgsub_filter_overlap_cpp, 1},
  {"_mgsub_get_matches_cpp", (DL_FUNC) &_mgsub_get_matches_cpp, 4},
  {NULL, NULL, 0}
};

void R_init_mgsub(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
