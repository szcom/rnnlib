/*Copyright 2009,2010 Alex Graves
 2014 Sergey Zyrianov

 This file is part of RNNLIB.

 RNNLIB is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 RNNLIB is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with RNNLIB.  If not, see <http://www.gnu.org/licenses/>.*/

#include "BivariateNorm.h"
#include <boost/math/constants/constants.hpp>
#include <cmath>
#include "Log.hpp"

using namespace boost::math;

namespace BivariateNorm {
real_t Z(real_t x, real_t y, real_t mu_x, real_t mu_y, real_t dev_x,
         real_t dev_y, real_t rho) {
  real_t divider = dev_x * dev_y;
  real_t rv = squared((x - mu_x) / dev_x) + squared((y - mu_y) / dev_y);
  real_t term = -2 * rho * (x - mu_x) * (y - mu_y) / divider;
  return rv + term;
}
}
