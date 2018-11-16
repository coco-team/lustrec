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

// functions of lustrec_math
void MPFRacos_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_acos(out, i, MPFR_RNDN);
}

void MPFRacosh_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_acosh(out, i, MPFR_RNDN);
}
void MPFRasin_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_asin(out, i, MPFR_RNDN);
}
void MPFRasinh_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_asinh(out, i, MPFR_RNDN);
}
void MPFRatan_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_atan(out, i, MPFR_RNDN);
}

void MPFRatan2_step (mpfr_t y, mpfr_t x, 
                           mpfr_t out
                           )
{
  mpfr_atan2(out, y, x, MPFR_RNDN);
}

void MPFRatanh_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_atanh(out, i, MPFR_RNDN);
}
void MPFRcbrt_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_cbrt(out, i, MPFR_RNDN);
}

void MPFRcos_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_cos(out, i, MPFR_RNDN);
}

void MPFRcosh_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_cosh(out, i, MPFR_RNDN);
}

void MPFRceil_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_ceil(out, i);
}

void MPFRerf_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_erf(out, i, MPFR_RNDN);
}

void MPFRexp_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_exp(out, i, MPFR_RNDN);
}

void MPFRfabs_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_abs(out, i, MPFR_RNDN);
}

void MPFRfloor_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_floor(out, i);
}

void MPFRfmod_step (mpfr_t i1, mpfr_t i2, 
                           mpfr_t out
                           )
{
  mpfr_fmod(out, i1, i2, MPFR_RNDN);
}

void MPFRlog_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_log(out, i, MPFR_RNDN);
}

void MPFRlog10_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_log10(out, i, MPFR_RNDN);
}

void MPFRpow_step (mpfr_t i1, mpfr_t i2, 
                           mpfr_t out
                           )
{
  mpfr_pow(out, i1, i2, MPFR_RNDN);
}

void MPFRround_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_round(out, i);
}

void MPFRsin_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_sin(out, i, MPFR_RNDN);
}

void MPFRsinh_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_sinh(out, i, MPFR_RNDN);
}

void MPFRsqrt_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_sqrt(out, i, MPFR_RNDN);
}

void MPFRtrunc_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_trunc(out, i);
}

void MPFRtan_step (mpfr_t i, 
                             mpfr_t out
                             )
{
  mpfr_tan(out, i, MPFR_RNDN);
}
