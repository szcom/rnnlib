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

#ifndef _INCLUDED_FullConnection_h
#define _INCLUDED_FullConnection_h

#include <vector>
#include <numeric>
#include <iostream>
#include <sstream>
#include "Layer.hpp"
#include "WeightContainer.hpp"
#include "Helpers.hpp"
#include "DataExporter.hpp"
#include "Connection.hpp"
#include "Matrix.hpp"

struct FullConnection : public Connection {
  // data
  vector<int> delay;
  vector<int> delayedCoords;
  FullConnection *source;
  pair<size_t, size_t> paramRange;

  // functions
  FullConnection(Layer *f, Layer *t,
                 const vector<int> &d = empty_list_of<int>(),
                 FullConnection *s = 0)
      : Connection(make_name(f, t, d), f, t), source(s),
        paramRange(source
                       ? source->paramRange
                       : WeightContainer::instance().new_parameters(
                             this->from->output_size() * this->to->input_size(),
                             this->from->name, this->to->name, name)) {
    if (source) {
      WeightContainer::instance().link_layers(this->from->name, this->to->name,
                                              this->name, paramRange.first,
                                              paramRange.second);
    }
    set_delay(d);
    assert(num_weights() ==
           (this->from->output_size() * this->to->input_size()));
    if (this->from->name != "bias" && this->from != this->to &&
        !this->to->source) {
      this->to->source = this->from;
    }
  }
  ~FullConnection() {}
  void set_delay(const vector<int> &d) {
    delay = d;
    assert(delay.size() == 0 || delay.size() == this->from->num_seq_dims());
    delayedCoords.resize(delay.size());
  }
  static const string &make_name(Layer *f, Layer *t, const vector<int> &d) {
    static string name;
    name = f->name + "_to_" + t->name;
    if (find_if(d.begin(), d.end(), std::bind2nd(not_equal_to<int>(), 0)) !=
        d.end()) {
      stringstream temp;
      temp << "_delay_";
      copy(d.begin(), d.end() - 1, ostream_iterator<int>(temp, "_"));
      temp << d.back();
      name += temp.str();
    }
    return name;
  }
  const View<real_t> weights() {
    return WeightContainer::instance().get_weights(paramRange);
  }
  const View<real_t> derivs() {
    return WeightContainer::instance().get_derivs(paramRange);
  }
  size_t num_weights() const { return difference(paramRange); }
  const vector<int> *add_delay(const vector<int> &toCoords) {
    if (delay.empty()) {
      return &toCoords;
    }
    range_plus(delayedCoords, toCoords, delay);
    if (this->from->outputActivations.in_range(delayedCoords)) {
      return &delayedCoords;
    }
    return 0;
  }
  void feed_forward(const vector<int> &toCoords) {
    const vector<int> *fromCoords = add_delay(toCoords);
    if (fromCoords) {
      dot(this->from->out_acts(*fromCoords), weights().begin(),
          this->to->inputActivations[toCoords]);
    }
  }
  void feed_back(const vector<int> &toCoords) {
    const vector<int> *fromCoords = add_delay(toCoords);
    if (fromCoords) {
      dot_transpose(this->to->inputErrors[toCoords], weights().begin(),
                    this->from->out_errs(*fromCoords));
    }
  }
  void update_derivs(const vector<int> &toCoords) {
    const vector<int> *fromCoords = add_delay(toCoords);
    if (fromCoords) {
      outer(this->from->out_acts(*fromCoords), derivs().begin(),
            this->to->inputErrors[toCoords]);
    }
  }
  void print(ostream &out) const {
    Named::print(out);
    out << " (" << num_weights() << " wts";
    if (source) {
      out << " shared with " << source->name;
    }
    out << ")";
  }
};

#endif
