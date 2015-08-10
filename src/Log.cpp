#include <cmath>
;
#include "Log.hpp"
#include "Helpers.hpp"

template <> const double Log<double>::logZero = -1e100;
template <> const float Log<float>::logZero = -1e30;
template <> const double Log<double>::logInfinity = 1e100;
template <> const float Log<float>::logInfinity = 1e30;

template <> float Log<float>::clip_exp(float x) {
  float t = bound(x, float(-87.0), float(88.0));
  return std::exp(t);
}
template <> double Log<double>::clip_exp(double x) {
  double t = bound(x, -500.0, 500.0);
  return std::exp(t);
}
