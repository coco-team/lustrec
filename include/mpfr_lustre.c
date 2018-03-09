#include <mpfr.h>
#include "mpfr_lustre.h"

void MPFR_LUSTRE_INIT () {
  return;
}

void MPFR_LUSTRE_CLEAR () {
  return;
}

void MPFRNeq_step (mpfr_t i1, mpfr_t i2, 
                          _Bool (*out)
                          )
{
  *out = mpfr_lessgreater_p(i1, i2);
}

void MPFREq_step (mpfr_t i1, mpfr_t i2, 
                         _Bool (*out)
                         )
{
  *out = mpfr_equal_p(i1, i2);
}

void MPFRGt_step (mpfr_t i1, mpfr_t i2, 
                         _Bool (*out)
                         )
{
  *out = mpfr_greater_p(i1, i2);
}

void MPFRGe_step (mpfr_t i1, mpfr_t i2, 
                         _Bool (*out)
                         )
{
  *out = mpfr_greaterequal_p(i1, i2);
}

extern void MPFRLt_step (mpfr_t i1, mpfr_t i2, 
                         _Bool (*out)
                         )
{
  *out = mpfr_less_p(i1, i2);
}
void MPFRLe_step (mpfr_t i1, mpfr_t i2, 
                         _Bool (*out)
                         )
{
  *out = mpfr_lessequal_p(i1, i2);
}

void MPFRDiv_step (mpfr_t i1, mpfr_t i2, 
                          mpfr_t out
                          )
{
  mpfr_div(out, i1, i2, MPFR_RNDN);
}

void MPFRTimes_step (mpfr_t i1, mpfr_t i2, 
                            mpfr_t out
                            )
{
  mpfr_mul(out, i1, i2, MPFR_RNDN);
}

void MPFRMinus_step (mpfr_t i1, mpfr_t i2, 
                            mpfr_t out
                            )
{
  mpfr_sub(out, i1, i2, MPFR_RNDN);
}

void MPFRPlus_step (mpfr_t i1, mpfr_t i2, 
                           mpfr_t out
                           )
{
  mpfr_add(out, i1, i2, MPFR_RNDN);
}

void MPFRUminus_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_neg(out, i, MPFR_RNDN);
}

void MPFRInit(mpfr_t i, mpfr_prec_t prec)
{
  mpfr_init2(i, prec);
}

void MPFRClear(mpfr_t i)
{
  mpfr_clear(i);
}
