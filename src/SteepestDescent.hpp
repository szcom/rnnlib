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

#ifndef _INCLUDED_SteepestDescent_h
#define _INCLUDED_SteepestDescent_h

#include <algorithm>
#include "Optimiser.hpp"
#include "DataExporter.hpp"

;
extern bool verbose;

struct SteepestDescent : public DataExporter, public Optimiser {
  // data
  ostream &out;
  vector<real_t> deltas;
  real_t learnRate;
  real_t momentum;

  // functions
  SteepestDescent(const string &name, ostream &o, vector<real_t> &weights,
                  vector<real_t> &derivatives, real_t lr = 1e-4,
                  real_t mom = 0.9)
      : DataExporter(name), Optimiser(weights, derivatives), out(o),
        learnRate(lr), momentum(mom) {
    build();
  }
  void update_weights() {
    assert(wts.size() == derivs.size());
    assert(wts.size() == deltas.size());
    LOOP(int i, indices(wts)) {
      real_t delta = (momentum * deltas[i]) - (learnRate * derivs[i]);
      deltas[i] = delta;
      wts[i] += delta;
    }
    if (verbose) {
      out << this->name << " weight updates:" << endl;
      PRINT(minmax(wts), out);
      PRINT(minmax(derivs), out);
      PRINT(minmax(deltas), out);
    }
  }
  // NOTE must be called after any change to weightContainer
  void build() {
    if (deltas.size() != wts.size()) {
      deltas.resize(wts.size());
      fill(deltas, 0);
      WeightContainer::instance().save_by_conns(deltas, name + "_deltas");
    }
  }
  void print(ostream &out = cout) const {
    out << "steepest descent" << endl;
    PRINT(learnRate, out);
    PRINT(momentum, out);
  }
};

#endif
