#include "simulink_math_fcn.h"
#include <math.h>


int rem_int (int x, int y) {
  return x%y;
}

int mod_int (int x, int y) {
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

double rem_real (double x, double y) {
  return fmod(x, y);
}

double mod_real (double x, double y) {
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
