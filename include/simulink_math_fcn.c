#include "simulink_math_fcn.h"
#include <math.h>

/* function exp_scalar_real (x: real) returns (y: real) prototype C lib m; */
double exp_scalar_real (double x) {
  return exp(x);
}


/* function log_scalar_real (x: real) returns (y: real) prototype C lib m; */
double log_scalar_real (double x) {
  return log(x);
}
/* function _10u_scalar_real (x: real) returns (y: real) prototype C lib m; */
double _10u_scalar_real (double x) {
  return pow(10.,x);
}

/* function log10_scalar_real (x: real) returns (y: real) prototype C lib m; */
double log10_scalar_real (double x) {
  return log10(x);
}

/* function magnitude_2_scalar_real (x: real) returns (y: real) prototype C lib m; */
double magnitude_2_scalar_real (double x) {
  return pow(fabs(x), 2.);
}

/* function square_scalar_real (x: real) returns (y: real) prototype C lib m; */
double square_scalar_real (double x) {
  return pow(x, 2.);
}

/* function pow_scalar_real (x,y: real) returns (z: real) prototype C lib m; */
double pow_scalar_real (double x, double y) {
  return pow(x, y);
}

/* function conj_scalar_real (x: real) returns (y: real) prototype C lib m; */
double conj_scalar_real (double x) {
  return x; // identity for real
}

/* function reciprocal_scalar_real (x: real) returns (y: real) prototype C lib m; */
double reciprocal_scalar_real (double x) {
  return 1./x; 
}

/* function hypot_scalar_real (x,y: real) returns (z: real) prototype C lib m; */
double hypot_scalar_real (double x, double y) {
  return sqrt(x*x + y*y); 
}



/*
  mod function produces a result that is either zero or has the same sign as the divisor.
  rem function produces a result that is either zero or has the same sign as the dividend.
  mod(a,0) returns a
  rem(a,0) returns NaN.

function rem_scalar_real_int_int_int (x,y: int) returns (z: int) prototype C lib m;
function rem_scalar_real_double_double_double (x,y: double) returns (z: double) prototype C lib m;
function rem_scalar_real_double_int_double (x: double; y: int) returns (z: double) prototype C lib m;
function rem_scalar_real_int_double_double (x: int; y: double) returns (z: double) prototype C lib m;

function mod_scalar_real_int_int_int (x,y: int) returns (z: int) prototype C lib m;
function mod_scalar_real_double_double_double (x,y: double) returns (z: double) prototype C lib m;
function mod_scalar_real_double_int_double (x: double; y: int) returns (z: double) prototype C lib m;
function mod_scalar_real_int_double_double (x: int; y: double) returns (z: double) prototype C lib m;
*/

int rem_scalar_real_int_int_int (int x, int y) {
  return x%y;
}

int mod_scalar_real_int_int_int (int x, int y) {
  int tmp;
  if (y == 0) { return x; };
  tmp = x%y;
  if (y < 0 && tmp > 0) {
    return tmp+y;
  }
  else {
    return tmp;
  }
}

double rem_scalar_real_double_double_double (double x, double y) {
  return fmod(x, y);
}

double mod_scalar_real_double_double_double (double x, double y) {
  double tmp = 0.;
  if (y == 0.) { return x; };
  tmp = fmod(x, y);
  if (y < 0. && tmp > 0.) {
    return tmp+y;
  }
  else {
    return tmp;
  }
}

double rem_scalar_real_double_int_double (double x, int y) {
  return rem_scalar_real_double_double_double (x, (double)y);
}

double rem_scalar_real_int_double_double (int x, double y) {
  return rem_scalar_real_double_double_double ((double)x, y);
}


double mod_scalar_real_double_int_double (double x, int y) {
  return (mod_scalar_real_double_double_double (x, (double)y));
}

double mod_scalar_real_int_double_double (int x, double y) {
  return (mod_scalar_real_double_double_double ((double)x, y));
}

/* function transpose_scalar_real (x: real) returns (y: real) prototype C lib m; */

/* function hermitian_scalar_real (x: real) returns (y: real) prototype C lib m; */
