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

#ifndef rnnlib_xcode_InputHiddenConnection_hpp
#define rnnlib_xcode_InputHiddenConnection_hpp

#include "FullConnection.hpp"

struct InputHiddenConnection : public FullConnection {
  InputHiddenConnection(Layer *f, Layer *t,
                        const vector<int> &d = empty_list_of<int>(),
                        FullConnection *s = 0)
      : FullConnection(f, t, d, s) {}
  void feed_forward(const vector<int> &toCoords) {
    if (0) {
      FullConnection::feed_forward(toCoords);
      return;
    }

    if (toCoords.size() != 1) {
      return;
    }
    if (toCoords[0] != 0) {
      return;
    }
    View<real_t> a = from->outputActivations.data.slice();
    View<real_t> b = to->inputActivations.data.slice();
    dot(a.begin(), a.end(), from->outputActivations.depth, weights().begin(),
        b.begin(), b.end(), to->inputActivations.depth);
  }
};

#endif
