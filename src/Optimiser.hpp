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

#ifndef _INCLUDED_Optimiser_h
#define _INCLUDED_Optimiser_h

#include <vector>
#include <algorithm>
#include "DataExporter.hpp"

;

struct Optimiser {
  // data
  vector<real_t> &wts;
  vector<real_t> &derivs;

  // functions
  Optimiser(vector<real_t> &weights, vector<real_t> &derivatives)
      : wts(weights), derivs(derivatives) {}
  virtual ~Optimiser() {}
  virtual void update_weights() = 0;
  virtual void print(ostream &out = cout) const = 0;
  virtual void build() = 0;
  virtual void reset_derivs() {}
};

ostream &operator<<(ostream &out, const Optimiser &o) {
  o.print(out);
  return out;
}

#endif
