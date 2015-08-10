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

#ifndef _INCLUDED_BlockLayer_h
#define _INCLUDED_BlockLayer_h

#include "Layer.hpp"

struct BlockLayer : public Layer {
  // data
  vector<size_t> blockShape;
  vector<int> blockOffset;
  vector<int> inCoords;
  size_t sourceSize;
  CVI blockIterator;
  vector<size_t> outSeqShape;

  // functions
  BlockLayer(Layer *src, const vector<size_t> &blockshape)
      : Layer(src->name + "_block", src->num_seq_dims(), 0,
              product(blockshape) * src->output_size(), src),
        blockShape(blockshape), blockOffset(this->num_seq_dims()),
        inCoords(this->num_seq_dims()),
        sourceSize(src->outputActivations.depth), blockIterator(blockShape),
        outSeqShape(this->num_seq_dims()) {
    assert(blockShape.size() == this->num_seq_dims());
    assert(!in(blockShape, 0));
    WeightContainer::instance().link_layers(this->source->name, this->name,
                                            this->source->name + "_to_" +
                                                this->name);
    display(this->outputActivations, "activations");
    display(this->outputErrors, "errors");
  }
  void print(ostream &out = cout) const {
    Layer::print(out);
    out << " block " << blockShape;
  }
  void start_sequence() {
    for (int i = 0; i < outSeqShape.size(); ++i) {
      outSeqShape.at(i) = ceil((real_t) this->source->output_seq_shape().at(i) /
                               (real_t)blockShape.at(i));
    }
    outputActivations.reshape(outSeqShape, 0);
    outputErrors.reshape(outputActivations, 0);
  }
  void feed_forward(const vector<int> &outCoords) {
    real_t *outIt = this->outputActivations[outCoords].begin();
    range_multiply(blockOffset, outCoords, blockShape);
    for (blockIterator.begin(); !blockIterator.end; ++blockIterator) {
      range_plus(inCoords, *blockIterator, blockOffset);
      View<real_t> inActs = this->source->outputActivations.at(inCoords);
      if (inActs.begin()) {
        copy(inActs.begin(), inActs.end(), outIt);
      } else {
        fill(outIt, outIt + sourceSize, 0);
      }
      outIt += sourceSize;
    }
  }
  void feed_back(const vector<int> &outCoords) {
    const real_t *outIt = this->outputErrors[outCoords].begin();
    range_multiply(blockOffset, outCoords, blockShape);
    for (blockIterator.begin(); !blockIterator.end; ++blockIterator) {
      range_plus(inCoords, *blockIterator, blockOffset);
      real_t *inErr = this->source->outputErrors.at(inCoords).begin();
      if (inErr) {
        transform(outIt, outIt + sourceSize, inErr, inErr, plus<real_t>());
      }
      outIt += sourceSize;
    }
  }
};

#endif
