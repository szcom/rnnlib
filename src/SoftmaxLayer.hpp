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

#ifndef _INCLUDED_SoftmaxLayer_h
#define _INCLUDED_SoftmaxLayer_h

#include <boost/algorithm/minmax_element.hpp>
#include "NetworkOutput.hpp"
#include "Log.hpp"

struct SoftmaxLayer : public FlatLayer {
  // data
  vector<string> targetLabels;
  SeqBuffer<Log<real_t> > logActivations;
  SeqBuffer<Log<real_t> > unnormedlogActivations;
  SeqBuffer<real_t> unnormedActivations;

  // functions
  SoftmaxLayer(const string &name, size_t numSeqDims,
               const vector<string> &labs)
      : FlatLayer(name, numSeqDims, labs.size()), targetLabels(labs),
        logActivations(this->output_size()),
        unnormedlogActivations(this->output_size()),
        unnormedActivations(this->output_size()) {
    display(this->inputErrors, "inputErrors", &targetLabels);
    display(this->outputErrors, "outputErrors", &targetLabels);
    display(this->inputActivations, "inputActivations", &targetLabels);
    display(this->outputActivations, "outputActivations", &targetLabels);
  }
  void start_sequence() {
    Layer::start_sequence();
    logActivations.reshape(this->inputActivations);
    unnormedlogActivations.reshape(logActivations);
    unnormedActivations.reshape(logActivations);
  }
  void feed_forward(const vector<int> &coords) {
    // transform to log scale and centre inputs on 0 for safer exponentiation
    View<Log<real_t> > unnormedLogActs = unnormedlogActivations[coords];
    real_t offset = pair_mean(minmax(this->inputActivations[coords]));
    LOOP(TDL t, zip(this->inputActivations[coords], unnormedLogActs)) {
      t.get<1>() = Log<real_t>(t.get<0>() - offset, true);
    }

    // apply exponential
    View<real_t> unnormedActs = unnormedActivations[coords];
    transform(unnormedLogActs, unnormedActs, mem_fun_ref(&Log<real_t>::exp));

    // normalise
    real_t Z = sum(unnormedActs);
    range_divide_val(this->outputActivations[coords], unnormedActs, Z);
    range_divide_val(logActivations[coords], unnormedLogActs, Log<real_t>(Z));
  }
  void feed_back(const vector<int> &coords) {
    View<real_t> outActs = this->outputActivations[coords];
    View<real_t> outErrs = this->outputErrors[coords];
    real_t Z = inner_product(outActs, outErrs);
    LOOP(TDDD t, zip(this->inputErrors[coords], outActs, outErrs)) {
      t.get<0>() = t.get<1>() * (t.get<2>() - Z);
    }
  }
};

#endif
