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

#ifndef _INCLUDED_DataSequence_h
#define _INCLUDED_DataSequence_h

#include <vector>
#include <iostream>
#include <iterator>
#include <string>
#include <boost/bimap.hpp>
#include "Helpers.hpp"
#include "SeqBuffer.hpp"

template <class R>
static string label_seq_to_str(const R &labelSeq,
                               const vector<string> &alphabet,
                               const string &delim = " ") {
  stringstream ss;
  for (typename range_const_iterator<R>::type it = boost::begin(labelSeq);
       it != boost::end(labelSeq); ++it) {
    if (in_range(alphabet, *it)) {
      ss << alphabet[*it];
    } else {
      ss << "<NULL>";
    }
    if (it != --boost::end(labelSeq)) {
      ss << delim;
    }
  }
  return ss.str();
}
static vector<int> str_to_label_seq(const string &labelSeqString,
                                    const vector<string> &alphabet) {
  static vector<int> v;
  v.clear();
  stringstream ss(labelSeqString);
  string lab;
  while (ss >> lab) {
    /*		check(in_right(alphabet, lab), lab + " not found in
     * alphabet");*/
    //		if (warn_unless(in_right(alphabet, lab), lab + " not found in
    // alphabet"))
    int i = index(alphabet, lab);
    if (i != alphabet.size()) {
      v += i;
    }
  }
  return v;
}

struct DataSequence {
  // data
  SeqBuffer<real_t> inputs;
  SeqBuffer<int> inputClasses;
  SeqBuffer<real_t> targetPatterns;
  SeqBuffer<int> targetClasses;
  SeqBuffer<real_t> importance;
  vector<int> targetLabelSeq;
  vector<string> targetWordSeq;
  string tag;
  string targetSentence;

  // functions
  DataSequence(const DataSequence &ds)
      : inputs(ds.inputs), inputClasses(ds.inputClasses),
        targetPatterns(ds.targetPatterns), targetClasses(ds.targetClasses),
        importance(ds.importance), targetLabelSeq(ds.targetLabelSeq),
        tag(ds.tag), targetSentence(ds.targetSentence) {}
  DataSequence(size_t inputDepth = 0, size_t targetPattDepth = 0)
      : inputs(inputDepth), inputClasses(0), targetPatterns(targetPattDepth),
        targetClasses(0), importance(0) {}
  size_t num_timesteps() const { return inputs.seq_size(); }
  void print(ostream &out, vector<string> *targetLabels = 0,
             vector<string> *inputLabels = 0) const {
    PRINT(tag, out);
    out << "input shape = (" << inputs.shape << ")" << endl;
    out << "timesteps = " << inputs.seq_size() << endl;
    if (!targetSentence.empty()) {
      out << "seq chars:" << targetSentence << endl;
    }
    if (targetLabelSeq.size() && targetLabels) {
      out << "target label sequence:" << endl;
      out << label_seq_to_str(this->targetLabelSeq, *targetLabels) << endl;
    }
    if (targetPatterns.size()) {
      out << "target shape = (" << targetPatterns.shape << ")" << endl;
    }
    if (verbose) {
      if (targetClasses.size() && targetLabels) {
        out << label_seq_to_str(this->targetClasses.data, *targetLabels)
            << endl;
      }
      if (inputClasses.size() && inputLabels) {
        out << label_seq_to_str(this->inputClasses.data, *inputLabels) << endl;
      }
    }
  }
};
static ostream &operator<<(ostream &out, const DataSequence &seq) {
  seq.print(out);
  return out;
}

#endif
