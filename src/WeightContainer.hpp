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

#ifndef _INCLUDED_WeightContainer_h
#define _INCLUDED_WeightContainer_h

#include <vector>
#include <numeric>
#include <algorithm>
#include <functional>
#include <math.h>
#include <boost/tuple/tuple.hpp>
#include "Random.hpp"
#include "DataExporter.hpp"

;

typedef multimap<string, tuple<string, string, int, int> >::iterator WC_CONN_IT;
typedef pair<string, tuple<string, string, int, int> > WC_CONN_PAIR;

struct WeightContainer : public DataExporter {
  // data
  Vector<real_t> weights;
  Vector<real_t> derivatives;
  multimap<string, tuple<string, string, int, int> > connections;

  // functions
  WeightContainer() : DataExporter("weightContainer") {}
  static WeightContainer &instance() {
    static WeightContainer wc;
    return wc;
  }
  void link_layers(const string &fromName, const string &toName,
                   const string &connName = "", int paramBegin = 0,
                   int paramEnd = 0) {
    connections.insert(
        make_pair(toName, boost::tuples::make_tuple(fromName, connName,
                                                    paramBegin, paramEnd)));
  }
  pair<size_t, size_t> new_parameters(size_t numParams, const string &fromName,
                                      const string &toName,
                                      const string &connName) {
    size_t begin = weights.size();
    weights.resize(weights.size() + numParams);
    size_t end = weights.size();
    link_layers(fromName, toName, connName, begin, end);
    return make_pair(begin, end);
  }
  View<real_t> get_weights(pair<int, int> range) {
    return weights.slice(range);
  }
  View<real_t> get_derivs(pair<int, int> range) {
    return derivatives.slice(range);
  }
  int randomise(real_t range) {
    int numRandWts = 0;
    LOOP(real_t & w, weights) {
      if (w == realInfinity) {
        w = Random::uniform(range);
        ++numRandWts;
      }
    }
    return numRandWts;
  }
  void reset_derivs() { fill(derivatives, 0); }
  void save_by_conns(vector<real_t> &container, const string &nam) {
    LOOP(const WC_CONN_PAIR & p, connections) {
      VDI begin = container.begin() + p.second.get<2>();
      VDI end = container.begin() + p.second.get<3>();
      if (begin != end) {
        save_range(make_pair(begin, end), p.second.get<1>() + "_" + nam);
      }
    }
  }
  // MUST BE CALLED BEFORE WEIGHT CONTAINER IS USED
  void build() {
    fill(weights, realInfinity);
    derivatives.resize(weights.size());
    save_by_conns(weights, "weights");
    reset_derivs();
  }
};
void perturb_weight(real_t &weight, real_t stdDev, bool additive = true) {
  weight += Random::normal(fabs(additive ? stdDev : stdDev * weight));
}
template <class R>
void perturb_weights(R &weights, real_t stdDev, bool additive = true) {
  LOOP(real_t & w, weights) { perturb_weight(w, stdDev, additive); }
}
template <class R>
void perturb_weights(R &weights, R &stdDevs, bool additive = true) {
  assert(boost::size(weights) == boost::size(stdDevs));
  LOOP(int i, indices(weights)) {
    perturb_weight(weights[i], stdDevs[i], additive);
  }
}

#endif
