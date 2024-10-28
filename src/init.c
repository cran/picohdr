
// #define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP zip_decompress_reorder_(SEXP vec_);
SEXP zip_compress_reorder_(SEXP raw_vec_);

static const R_CallMethodDef CEntries[] = {
  
  {"zip_decompress_reorder_"  , (DL_FUNC) &zip_decompress_reorder_  , 1},
  {"zip_compress_reorder_"  , (DL_FUNC) &zip_compress_reorder_  , 1},
  {NULL , NULL, 0}
};


void R_init_picohdr(DllInfo *info) {
  R_registerRoutines(
    info,      // DllInfo
    NULL,      // .C
    CEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(info, FALSE);
}



