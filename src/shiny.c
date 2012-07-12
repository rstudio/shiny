#include "shiny.h"

#include <R_ext/Rdynload.h>
#include <sys/time.h>

static R_CallMethodDef CallEntries[] = {
  {"getTimeInMillis", (DL_FUNC)&getTimeInMillis, 0},
  {NULL, NULL, 0}
};

void R_init_shiny(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

SEXP getTimeInMillis(void)
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  
  return Rf_ScalarReal(tv.tv_sec * 1000 + (tv.tv_usec / 1000.0));
}
