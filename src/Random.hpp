/*Copyright 2009,2010 Alex Graves

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

#ifndef _INCLUDED_Random_h
#define _INCLUDED_Random_h

#include "Helpers.hpp"

namespace Random {
unsigned int set_seed(unsigned int seed = 0);
real_t normal(); // normal distribution with mean 0 std dev 1
real_t normal(real_t dev, real_t mean = 0); // normal distribution with user
                                            // defined mean, dev
real_t uniform(real_t range);               // uniform real in (-range, range)
real_t uniform();                           // uniform real in (0,1)
PDD binormal(real_t dev_x, real_t mean_x, real_t dev_y, real_t mean_y,
             real_t rho);
bool bernoulli(real_t p);
}

#endif
