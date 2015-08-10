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

#ifndef _INCLUDED_CopyConnection_h
#define _INCLUDED_CopyConnection_h

#include "Connection.hpp"

struct CopyConnection : public Connection {
  // functions
  CopyConnection(Layer *f, Layer *t)
      : Connection(f->name + "_to_" + t->name, f, t) {
    assert(this->from != this->to);
    assert(this->from->output_size() == this->to->input_size());
    assert(this->from->output_size());
    this->to->source = this->from;
    WeightContainer::instance().link_layers(this->from->name, this->to->name);
  }
  virtual ~CopyConnection() {}
  void feed_forward(const vector<int> &coords) {
    range_plus_equals(this->to->inputActivations[coords],
                      this->from->outputActivations[coords]);
  }
  void feed_back(const vector<int> &coords) {
    range_plus_equals(this->from->outputErrors[coords],
                      this->to->inputErrors[coords]);
  }
  void print(ostream &out) const {
    Named::print(out);
    out << " (copy)";
  }
};

#endif
