/*Copyright 2009-2011 Alex Graves

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

#ifndef _INCLUDED_DatasetErrors_h
#define _INCLUDED_DatasetErrors_h

#include "Helpers.hpp"
#include "String.hpp"

struct DatasetErrors {
  // data
  map<string, real_t> errors;
  map<string, real_t> normFactors;
  static set<string> percentErrors;

  // functions
  DatasetErrors() {}
  void clear() {
    errors.clear();
    normFactors.clear();
  }
  void add_error(string name, real_t error, real_t normFactor = 1) {
    CHECK_STRICT(boost::math::isfinite(error),
                 name + " has value " + str(error));
    errors[name] += error;
    normFactors[name] += normFactor;
  }
  void add_seq_errors(const map<string, real_t> &seqErrors,
                      const map<string, real_t> &seqNorms) {
    LOOP(const PSD & p, seqErrors) {
      add_error(p.first, p.second,
                in(seqNorms, p.first) ? at(seqNorms, p.first) : 1);
    }
  }
  void normalise() {
    LOOP(PCSD & p, errors) {
      real_t normFactor = normFactors[p.first];
      if (normFactor) {
        p.second /= normFactor;
        if (percent_error(p.first))
            //				if (in(percentErrors, p.first)/* ||
            // p.first[0]
            //==
            //'_'*/)
        {
          p.second *= 100;
        }
      }
      normFactors[p.first] = 0;
    }
  }
  void print(ostream &out) const {
    LOOP(const PSD & p, errors) {
      out << p;
      if (percent_error(p.first))
          //			if (in(percentErrors, p.first)/* || p.first[0]
          //==
          //'_'*/)
      {
        out << "%";
      }
      out << endl;
    }
  }
  bool percent_error(const string &err) const {
    LOOP(const string & pErr, percentErrors) {
      if (in(lower(err), lower(pErr))) {
        return true;
      }
    }
    return false;
  }
};
set<string> DatasetErrors::percentErrors =
    list_of("classificationError")("wordError")("labelError")("seqError")(
        "deletions")("insertions")("substitutions")("ratio");

static ostream &operator<<(ostream &out, const DatasetErrors &de) {
  de.print(out);
  return out;
}

typedef pair<const string, pair<int, DatasetErrors> > PSPIDE;

#endif
