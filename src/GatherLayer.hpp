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

#ifndef _INCLUDED_GatherLayer_h
#define _INCLUDED_GatherLayer_h

#include "Layer.hpp"

struct GatherLayer : public Layer {
  // data
  vector<Layer *> sources;

  // functions
  GatherLayer(const string &name, vector<Layer *> &srcs)
      : Layer(name, srcs.front()->num_seq_dims(), 0, get_size(srcs),
              srcs.front()),
        sources(srcs) {
    source = sources.front();
    WeightContainer::instance().new_parameters(0, source->name, name,
                                               source->name + "_to_" + name);
    display(outputActivations, "activations");
    display(outputErrors, "errors");
  }
  int get_size(vector<Layer *> &srcs) {
    int size = 0;
    for (int i = 0; i < srcs.size(); ++i) {
      size += srcs[i]->output_size();
    }
    return size;
  }
  void feed_forward(const vector<int> &outCoords) {
    real_t *actBegin = outputActivations[outCoords].begin();
    LOOP(Layer * l, sources) {
      View<real_t> inActs = l->outputActivations[outCoords];
      copy(inActs.begin(), inActs.end(), actBegin);
      actBegin += inActs.size();
    }
  }
  void feed_back(const vector<int> &outCoords) {
    real_t *errBegin = outputErrors[outCoords].begin();
    LOOP(Layer * l, sources) {
      View<real_t> inErrs = l->outputErrors[outCoords];
      int dist = inErrs.size();
      copy(errBegin, errBegin + dist, inErrs.begin());
      errBegin += dist;
    }
  }
};

#endif
