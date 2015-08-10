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

#ifndef _INCLUDED_InputLayer_h
#define _INCLUDED_InputLayer_h

#include "Layer.hpp"

struct InputLayer : public Layer {
  // functions
  InputLayer(const string &name, size_t numSeqDims, size_t size,
             const vector<string> &inputLabels)
      : Layer(name, numSeqDims, 0, size) {
    const vector<string> *labs = inputLabels.empty() ? 0 : &inputLabels;
    display(this->outputActivations, "activations", labs);
    display(this->outputErrors, "errors", labs);
  }
  ~InputLayer() {}
  template <typename T> void copy_inputs(const SeqBuffer<T> &inputs) {
    assert(inputs.depth == this->output_size());
    this->outputActivations = inputs;
    this->outputErrors.reshape(this->outputActivations, 0);
  }
};

#endif
