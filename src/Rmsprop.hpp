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

#ifndef rnnlib_xcode_Rmsprop_hpp
#define rnnlib_xcode_Rmsprop_hpp
#include <algorithm>
#include "Optimiser.hpp"
#include "DataExporter.hpp"
extern bool verbose;

struct Rmsprop : public DataExporter, public Optimiser {
  // data
  ostream &out;
  vector<real_t> deltas;
  vector<real_t> n;
  vector<real_t> g;

  real_t learnRate;
  real_t momentum;
  real_t ngMomemtum;
  real_t minDelta;
  real_t maxDelta;

  // functions
  Rmsprop(const string &name, ostream &o, vector<real_t> &weights,
          vector<real_t> &derivatives, real_t lr = 1e-4, real_t mom = 0.9,
          real_t ngMom = 0.95)
      : DataExporter(name), Optimiser(weights, derivatives), out(o),
        learnRate(lr), momentum(mom), ngMomemtum(ngMom), minDelta(-0.2),
        maxDelta(0.2) {
    build();
  }
  void update_weights() {
    assert(wts.size() == derivs.size());
    assert(wts.size() == deltas.size());
    LOOP(int i, indices(wts)) {
      real_t devi = derivs[i];
      real_t ni = ngMomemtum * n[i] + (1 - ngMomemtum) * squared(devi);
      real_t gi = ngMomemtum * g[i] + (1 - ngMomemtum) * devi;
      n[i] = ni;
      g[i] = gi;

      real_t d_delta = (learnRate * devi) / sqrt(ni - squared(gi) + learnRate);
      real_t delta = (momentum * deltas[i]) - d_delta;
      CHECK_STRICT(boost::math::isfinite(delta), "delta is inf");
      deltas[i] = delta;
      wts[i] += delta;
    }
    static unsigned weightDisplayCnt = 0;
    if (verbose && (weightDisplayCnt++ % 1000) == 0) {
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
      n.resize(wts.size());
      fill(n, 0);
      g.resize(wts.size());
      fill(g, 0);
      WeightContainer::instance().save_by_conns(deltas, name + "_deltas");
      WeightContainer::instance().save_by_conns(n, name + "_n");
      WeightContainer::instance().save_by_conns(g, name + "_g");
    }
  }
  virtual void reset_derivs() {
    fill(deltas, 0);
    fill(n, 0);
    fill(g, 0);
  }
  void print(ostream &out = cout) const {
    out << "rmsprop" << endl;
    PRINT(learnRate, out);
    PRINT(momentum, out);
  }
};

#endif
