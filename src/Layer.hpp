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

#ifndef _INCLUDED_Layer_h
#define _INCLUDED_Layer_h

#include <boost/assign/list_of.hpp>
#include "SeqBuffer.hpp"
#include "DataExporter.hpp"

extern bool verbose;

struct Layer : public DataExporter {
  // typedefs
  //	typedef multi_array<real_t, 3> array3d;

  // data
  vector<int> directions;
  SeqBuffer<real_t> inputActivations;
  SeqBuffer<real_t> outputActivations;
  SeqBuffer<real_t> inputErrors;
  SeqBuffer<real_t> outputErrors;
  Layer *source;

  // functions
  Layer(const string &name, size_t numSeqDims, size_t inputSize,
        size_t outputSize, Layer *src = 0)
      : DataExporter(name), inputActivations(inputSize),
        outputActivations(outputSize), inputErrors(inputSize),
        outputErrors(outputSize), source(src) {
    assert(inputSize || outputSize);
    directions.resize(numSeqDims, 1);
  }
  Layer(const string &name, const vector<int> &dirs, size_t inputSize,
        size_t outputSize, Layer *src = 0)
      : DataExporter(name), directions(dirs), inputActivations(inputSize),
        outputActivations(outputSize), inputErrors(inputSize),
        outputErrors(outputSize), source(src) {
    assert(inputSize || outputSize);
    LOOP(int d, directions) { assert(d == 1 || d == -1); }
  }
  virtual ~Layer() {}
  virtual size_t input_size() const { return inputActivations.depth; }
  virtual size_t output_size() const { return outputActivations.depth; }
  virtual size_t num_seq_dims() const { return directions.size(); }
  virtual const View<const size_t> output_seq_shape() const {
    return outputActivations.seq_shape();
  }
  virtual const View<const size_t> input_seq_shape() const {
    return input_size() ? inputActivations.seq_shape() : output_seq_shape();
  }
  virtual SeqIterator output_seq_begin() const {
    return outputActivations.begin(directions);
  }
  virtual SeqIterator input_seq_begin() const {
    return input_size() ? inputActivations.begin(directions)
                        : outputActivations.begin(directions);
  }
  virtual SeqIterator input_seq_rbegin() const {
    return input_size() ? inputActivations.rbegin(directions)
                        : outputActivations.rbegin(directions);
  }
  virtual void print(ostream &out = cout) const {
    out << typeid(*this).name() << " ";
    Named::print(out);
    out << " " << num_seq_dims() << "D";
    if (directions.size()) {
      out << " (";
      LOOP(int d, directions) { out << ((d > 0) ? "+" : "-"); }
      out << ")";
    }
    if (input_size() == 0) {
      out << " size " << output_size();
    } else if (output_size() == 0 || input_size() == output_size()) {
      out << " size " << input_size();
    } else {
      out << " inputSize " << input_size() << " outputSize " << output_size();
    }

    if (source) {
      out << " source \"" << source->name << "\"";
    }
  }
  virtual void build() { assert(source); }
  virtual void reshape_errors() {
    inputErrors.reshape(inputActivations, 0);
    outputErrors.reshape(outputActivations, 0);
  }
  virtual void start_sequence() {
    assert(!in(source->output_seq_shape(), 0));
    inputActivations.reshape(source->output_seq_shape(), 0);
    outputActivations.reshape(source->output_seq_shape(), 0);
    reshape_errors();
  }
  virtual const View<real_t> out_acts(const vector<int> &coords) {
    return outputActivations[coords];
  }
  virtual const View<real_t> out_errs(const vector<int> &coords) {
    return outputErrors[coords];
  }
  virtual void feed_forward(const vector<int> &coords) {}
  virtual void feed_back(const vector<int> &coords) {}
  virtual void update_derivs(const vector<int> &coords) {}
  virtual const View<real_t> weights() { return View<real_t>(); }
  virtual void set_dropout_mode(bool is_training) {}
};

ostream &operator<<(ostream &out, const Layer &l) {
  l.print(out);
  return out;
}

struct FlatLayer : public Layer {
  FlatLayer(const string &name, size_t numSeqDims, size_t size, Layer *src = 0)
      : Layer(name, numSeqDims, size, size, src) {}
  FlatLayer(const string &name, const vector<int> &dirs, size_t size,
            Layer *src = 0)
      : Layer(name, dirs, size, size, src) {}
};

#endif
