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
    if (x == 0.0 || y == 0.0){
        return 0.0;
    }else{
        return fmod(x, y);
    }
}

double mod_real (double x, double y) {
  double tmp = 0.;
  if (y == 0.) { return x; };
  if (x == 0.) { return 0; };
  tmp = fmod(x, y);
  if (y*tmp < 0.) {
    return tmp+y;
  }
  else {
    return tmp;
  }
}
