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

#ifndef _INCLUDED_NetcdfDataset_h
#define _INCLUDED_NetcdfDataset_h

#include <functional>
#include <algorithm>
#include <string>
#include <numeric>
#include <map>
#include "netcdfcpp.h"
#include <boost/algorithm/string/replace.hpp>
#include "DataSequence.hpp"
#include "Helpers.hpp"

#define SEQ_IT vector<DataSequence *>::iterator
#define CONST_SEQ_IT vector<DataSequence *>::const_iterator
static int load_nc_dim(const NcFile &ncf, const string &name,
                       bool required = true) {
  NcDim *d = 0;
  try {
    d = ncf.get_dim(name.c_str());
  }
  catch (char *str) {
    check(!required,
          string(str) + "\ndimension " + name + " not found in netcdf file");
  }
  int size = d ? d->size() : 0;
  return size;
}
static NcVar *load_nc_variable(const NcFile &ncf, const string &name,
                               bool required = true) {
  NcVar *v = 0;
  try {
    v = ncf.get_var(name.c_str());
  }
  catch (char *str) {
    check(!required,
          string(str) + "\nvariable " + name + " not found in netcdf file");
  }
  return v;
}
bool find_nc_variable(const NcFile &ncf, const string &name) {
  NcVar *v = load_nc_variable(ncf, name, false);
  bool ret = (v != 0);
  return ret;
}
static string get_nc_string(const NcFile &ncf, const string &name,
                            int offset = 0, bool required = true) {
  static array<long, 2> offsets = { { 0, 0 } };
  static array<long, 2> counts = { { 1, 0 } };
  NcVar *v = load_nc_variable(ncf, name.c_str(), required);
  if (v) {
    long *shape = v->edges();
    offsets.front() = offset;
    counts.back() = shape[1];
    v->set_cur(&offsets.front());
    char *temp = new char[shape[1]];
    delete[] shape;
    bool success = v->get(temp, &counts.front());
    if (!success) {
      check(!required, " index " + str(offset) + " out of bounds for " + name +
                           " in netcdf file");
    }
    string s(temp);
    delete[] temp;
    return s;
  }
  return "";
}
template <class T>
static bool load_nc_array(const NcFile &ncf, const string &name,
                          vector<T> &dest, bool required = true, int offset = 0,
                          int count = -1) {
  NcVar *v = load_nc_variable(ncf, name.c_str(), required);
  if (v) {
    vector<long> offsets = list_of(offset).repeat(v->num_dims() - 1, 0);
    v->set_cur(&offsets.front());
    vector<long> counts(v->num_dims());
    long *shape = v->edges();
    transform(shape, shape + v->num_dims(), offsets.begin(), counts.begin(),
              minus<long>());
    delete[] shape;
    if (count > 0) {
      counts[0] = count;
    }
    dest.resize(product(counts));
    bool success = v->get(&dest.front(), &counts.front());
    if (!success) {
      dest.resize(0);
      check(!required, string("NetcdfDataset::load_nc_array<") +
                           typeid(T).name() + "> " + name + '\n' +
                           "failed with offset " + str(offsets) + ", counts " +
                           str(counts));
    }
    return success;
  }
  return false;
}
template <class T>
static vector<T> get_nc_array_step(const NcFile &ncf, const string &name,
                                   int offset = 0, bool required = true) {
  vector<T> dest;
  load_nc_array(ncf, name, dest, required, offset, 1);
  return dest;
}

struct DataHeader {
  // data
  int numDims;
  Vector<string> inputLabels;
  map<string, int> inputLabelCounts;
  Vector<string> targetLabels;
  map<string, int> targetLabelCounts;
  size_t inputSize;
  size_t outputSize;
  size_t numSequences;
  size_t numTimesteps;
  size_t totalTargetStringLength;

  // functions
  DataHeader(const string &filename, const string &task, real_t dataFraction)
      : outputSize(0), numTimesteps(0), totalTargetStringLength(0) {
    NcFile nc(filename.c_str(), NcFile::ReadOnly);
    check(nc.is_valid(), "can't open data file " + filename);
    NcError err(NcError::silent_nonfatal);
    numDims = load_nc_dim(nc, "numDims", false);
    if (!numDims) {
      numDims = 1;
    }
    inputSize = load_nc_dim(nc, "inputPattSize");
    int maxSeqs = load_nc_dim(nc, "numSeqs");
    numSequences = bound((int)(dataFraction * maxSeqs), 1, maxSeqs);
    LOOP(int s, span(numSequences)) {
      vector<int> seqDims =
          get_nc_array_step<int>(nc, "seqDims", s, numDims != 1);
      if (seqDims.empty()) {
        seqDims = get_nc_array_step<int>(nc, "seqLengths", s);
      }
      numTimesteps += product(seqDims);
    }
    if (in(task, "regression")) {
      outputSize = load_nc_dim(nc, "targetPattSize");
    } else if (task == "memory" || task == "code") {
      outputSize = inputSize;
    } else if (task == "classification" || task == "sequence_classification" ||
               in(task, "transcription")) {
      outputSize = load_nc_dim(nc, "numLabels");
    }
    if (get_nc_string(nc, "inputLabels", 0, in(task, "discrete")) != "") {
      int numInputLabels = load_nc_dim(nc, "numInputLabels");
      for (int i = 0; i < numInputLabels; ++i) {
        string label = get_nc_string(nc, "inputLabels", i);
        replace_all(label, "_", "-");
        inputLabels += label;
        inputLabelCounts[label] = 0;
      }
    }
    for (int i = 0; i < outputSize; ++i) {
      string label = get_nc_string(nc, "labels", i);
      if (label == "") {
        label = int_to_sortable_string(i, outputSize);
      }
      targetLabels += label;
      targetLabelCounts[label] = 0;
    }
    if (in(task, "discrete")) {
      vector<int> inputClasses;
      load_nc_array(nc, "inputClasses", inputClasses);
      LOOP(int c, inputClasses) {
        if (c >= 0) {
          check(in_range(inputLabels, c), "input class index " + str(c) +
                                              " not in range of input labels");
          ++inputLabelCounts[inputLabels[c]];
        }
      }
    }
    if (task == "classification") {
      vector<int> targetClasses;
      load_nc_array(nc, "targetClasses", targetClasses);
      LOOP(int c, targetClasses) {
        if (c >= 0) {
          check(in_range(targetLabels, c),
                "target class index " + str(c) +
                    " not in range of target alphabet");
          ++targetLabelCounts[targetLabels[c]];
        }
      }
    } else if (find_nc_variable(nc, "targetStrings") &&
               (task == "transcription" || task == "sequence_classification")) {
      LOOP(int s, span(numSequences)) {
        stringstream targetString(get_nc_string(nc, "targetStrings", s));
        string label;
        while (targetString >> label) {
          // check(in_right(targetLabels, label), "label \'" + label + "\' in
          // \'" + targetString.str() + "\' not found in target alphabet");
          if (warn_unless(in(targetLabels, label),
                          "label \'" + label + "\' in \'" + targetString.str() +
                              "\' not found in target alphabet")) {
            ++targetLabelCounts[label];
            ++totalTargetStringLength;
          }
          if (task == "sequence_classification") {
            break;
          }
        }
      }
    }
    check(targetLabelCounts.size() == targetLabels.size(),
          str(targetLabels.size()) + " target labels and " +
              str(targetLabelCounts.size()) +
              " target label counts (should be equal)");
    check(inputLabelCounts.size() == inputLabels.size(),
          str(inputLabels.size()) + " input labels and " +
              str(inputLabelCounts.size()) +
              " input label counts (should be equal)");
  }
  void print(ostream &out) const {
    PRINT(numDims, out);
    PRINT(inputSize, out);
    if (outputSize > 0) {
      PRINT(outputSize, out);
    }
    PRINT(numSequences, out);
    PRINT(numTimesteps, out);
    if (targetLabels.size()) {
      prt_line(out);
      out << targetLabels.size() << " target labels:" << endl;
      LOOP(TIS t, enumerate(targetLabels)) {
        out << t << " (" << at(targetLabelCounts, get<1>(t)) << ")" << endl;
      }
    }
    if (inputLabels.size()) {
      prt_line(out);
      out << inputLabels.size() << " input labels:" << endl;
      LOOP(TIS t, enumerate(inputLabels)) {
        out << t << " (" << at(inputLabelCounts, get<1>(t)) << ")" << endl;
      }
    }
  }
};

static ostream &operator<<(ostream &out, const DataHeader &dh) {
  dh.print(out);
  return out;
}

struct NetcdfDataset {
  // data
  NcFile nc;
  string filename;
  string task;
  DataHeader header;
  vector<DataSequence *> sequences;
  SeqBuffer<int> inputSeqDims;
  SeqBuffer<int> targetSeqDims;
  NcError err;

  // functions
  NetcdfDataset(const string &fname, const string &t, real_t dataFraction = 1.0)
      : nc(fname.c_str(), NcFile::ReadOnly), filename(fname), task(t),
        header(filename, task, dataFraction), err(NcError::silent_nonfatal) {
    init();
    int maxSeqs = load_nc_dim(nc, "numSeqs");
    int numSeqs = bound((int)(dataFraction * maxSeqs), 1, maxSeqs);
    load_sequences(0, numSeqs);
  }
  NetcdfDataset(const string &fname, const string &t, int seqNum)
      : nc(fname.c_str(), NcFile::ReadOnly), filename(fname), task(t),
        header(filename, task, 0), err(NcError::silent_nonfatal) {
    init();
    load_sequences(seqNum, seqNum + 1);
  }
  NetcdfDataset(const string &fname, const string &t, const string &dataBin)
      : nc(fname.c_str(), NcFile::ReadOnly), filename(fname), task(t),
        header(filename, task, 0), err(NcError::silent_nonfatal) {
    init();
    int maxSeqs = load_nc_dim(nc, "numSeqs");
    int binSize = maxSeqs / dataBin.length();
    int left = binSize * dataBin.find("1");
    CHECK_STRICT(left >= 0 && left < maxSeqs, "seq number out of bounds");
    int right = bound((int)(left + binSize), 1, maxSeqs);
    if (right + binSize > maxSeqs) {
      right = maxSeqs;
    }
    load_sequences(left, right);
  }
  ~NetcdfDataset() { delete_range(sequences); }
  void init() {
    check(nc.is_valid(), "can't open data file " + filename);
    inputSeqDims.reshape_with_depth(list_of<int>(load_dim("numSeqs")),
                                    header.numDims);
    if (!load_array("seqDims", inputSeqDims.data, header.numDims != 1)) {
      load_array("seqLengths", inputSeqDims.data);
    }
    targetSeqDims.reshape_with_depth(list_of<int>(load_dim("numSeqs")),
                                     header.numDims);
    if (!load_array("targetSeqDims", targetSeqDims.data)) {
      targetSeqDims = inputSeqDims;
    }
  }
  int size() const { return sequences.size(); }
  void shuffle_sequences() { shuffle(sequences); }
  DataSequence &operator[](int n) { return *sequences.at(n); }
  size_t timesteps() const {
    size_t total = 0;
    LOOP(const DataSequence * seq, sequences) {
      total += seq->inputs.seq_size();
    }
    return total;
  }
  pair<int, int> seq_to_offset(int seqNum) const {
    return make_pair(product(inputSeqDims[seqNum]),
                     product(targetSeqDims[seqNum]));
  }
  pair<int, int> get_offset(int seqNum) const {
    pair<int, int> offset(0, 0);
    LOOP(int i, span(seqNum)) { offset += seq_to_offset(i); }
    return offset;
  }
  void load_sequences(int first, int last) {
    COUT << "loading sequences from " << first << " to " << last << endl;
    pair<int, int> offsets = get_offset(first);
    LOOP(int i, span(first, last)) {
      check(i >= 0 && i < inputSeqDims.shape[0],
            "sequence " + str(i) + " requested from data file " +
                str(filename) + " containing " + str(inputSeqDims.shape[0]) +
                " sequences");
      DataSequence *seq = new DataSequence(
          header.inputSize, in(task, "regression") ? header.outputSize : 0);
      vector<int> inputShape = flip(inputSeqDims[i]);
      int inputCount = product(inputShape);
      vector<int> targetShape = flip(targetSeqDims[i]);
      int targetCount = product(targetShape);
      load_to_seq_buffer(seq->inputs, inputShape, "inputs", true, offsets.first,
                         inputCount);
      if (find_variable("importance")) {
        load_to_seq_buffer_with_depth(seq->importance, targetShape, 1,
                                      "importance", true, offsets.second,
                                      targetCount);
      }
      if (in(task, "regression")) {
        if (task == "sequence_regression") {
          targetShape.clear();
        }
        load_to_seq_buffer(seq->targetPatterns, targetShape, "targetPatterns",
                           true, offsets.second, targetCount);
      } else if (in(task, "prediction")) {
        seq->targetPatterns.reshape_with_depth(targetShape, seq->inputs.depth);
        load_to_seq_buffer(seq->targetPatterns, targetShape, "targetPatterns",
                           true, offsets.second, targetCount);
        if (find_variable("targetStrings")) {
          seq->targetSentence = get_string("targetStrings", i);
        }
      } else if (task == "classification") {
        load_to_seq_buffer_with_depth(seq->targetClasses, targetShape, 1,
                                      "targetClasses", true, offsets.second,
                                      targetCount);
      } else if (in(task, "discrete")) {
        load_to_seq_buffer_with_depth(seq->inputClasses, inputShape, 1,
                                      "inputClasses", true, offsets.second,
                                      inputCount);
      } else if (task == "sequence_classification") {
        if (find_variable("targetStrings")) {
          seq->targetLabelSeq = str_to_label_seq(get_string("targetStrings", i),
                                                 header.targetLabels);
          seq->targetClasses.reshape_with_depth(empty_list_of<size_t>(), 1);
          seq->targetClasses.get(list_of(0)) = seq->targetLabelSeq[0];
        } else {
          load_to_seq_buffer_with_depth(
              seq->targetClasses, empty_list_of<size_t>(), 1, "targetClasses",
              true, offsets.second, 1);
        }
      } else if (in(task, "transcription")) {
        if (find_variable("wordTargetStrings")) {
          seq->targetWordSeq =
              split<string>(get_string("wordTargetStrings", i));
        }
        seq->targetLabelSeq = str_to_label_seq(get_string("targetStrings", i),
                                               header.targetLabels);
      }
      seq->tag = get_string("seqTags", i, false);
      sequences.push_back(seq);
      offsets += make_pair(inputCount, targetCount);
    }
  }
  bool find_variable(const string &name) {
    NcVar *v = load_variable(name, false);
    bool ret = (v != 0);
    return ret;
  }
  NcVar *load_variable(const string &name, bool required = true) {
    return load_nc_variable(nc, name, required);
  }
  int load_dim(const string &name, bool required = true) {
    return load_nc_dim(nc, name, required);
  }
  string get_string(const string &name, int offset = 0, bool required = true) {
    return get_nc_string(nc, name, offset, required);
  }
  template <class T, class R>
  bool load_to_seq_buffer(SeqBuffer<T> &dest, const R &shape,
                          const string &name, bool required = true,
                          int offset = 0, int count = -1) {
    dest.reshape(shape);
    return load_array(name, dest.data, required, offset, count);
  }
  template <class T, class R>
  bool load_to_seq_buffer_with_depth(SeqBuffer<T> &dest, const R &shape,
                                     int depth, const string &name,
                                     bool required = true, int offset = 0,
                                     int count = -1) {
    dest.reshape_with_depth(shape, depth);
    return load_array(name, dest.data, required, offset, count);
  }
  template <class T>
  bool load_array(const string &name, vector<T> &dest, bool required = true,
                  int offset = 0, int count = -1) {
    return load_nc_array<T>(nc, name, dest, required, offset, count);
  }
  void print(ostream &out) const {
    PRINT(filename, out);
    out << sequences.size() << " sequences" << endl;
    out << timesteps() << " timesteps" << endl;
    header.print(out);
  }
};

static ostream &operator<<(ostream &out, const NetcdfDataset &d) {
  d.print(out);
  return out;
}

struct DataList {
  // data
  vector<string> filenames;
  vector<DataHeader> headers;
  map<string, real_t> inputLabelHits;
  map<string, real_t> targetLabelCounts;
  string task;
  size_t numSequences;
  size_t numTimesteps;
  size_t totalTargetStringLength;
  NetcdfDataset *dataset;
  int datasetIndex;
  DataSequence *seq;
  int seqIndex;
  real_t dataFraction;
  bool shuffled;
  std::string dataBin;

  // functions
  DataList(const vector<string> &filenams, const string &t, bool shuffle,
           real_t loadFrac, const std::string &bin = "")
      : filenames(filenams), task(t), numSequences(0), numTimesteps(0),
        totalTargetStringLength(0), dataset(0), datasetIndex(-1), seq(0),
        seqIndex(-1), dataFraction(loadFrac), shuffled(shuffle), dataBin(bin) {
    LOOP(TIS t, enumerate(filenames)) {
      headers += DataHeader(t.get<1>(), task, loadFrac);
      const DataHeader &curr = headers.back();
      numSequences += curr.numSequences;
      numTimesteps += curr.numTimesteps;
      totalTargetStringLength += curr.totalTargetStringLength;
      targetLabelCounts += curr.targetLabelCounts;
      inputLabelHits += curr.inputLabelCounts;
      if (t.get<0>()) {
        const DataHeader &prev = nth_last(headers, 2);
        assert(prev.numDims == curr.numDims);
        assert(prev.targetLabels == curr.targetLabels);
        assert(prev.inputSize == curr.inputSize);
        assert(prev.outputSize == curr.outputSize);
      }
    }
  }
  ~DataList() { delete_dataset(); }
  void clear_seq() {
    seqIndex = -1;
    seq = 0;
  }
  void delete_dataset() {
    delete dataset;
    dataset = 0;
    clear_seq();
  }
  bool next_dataset() {
    if (dataset && filenames.size() > 1) {
      delete_dataset();
    }
    if (datasetIndex >= (int)last_index(filenames)) {
      datasetIndex = -1;
      return true;
    }
    ++datasetIndex;
    if (!dataset) {
      dataset =
          dataBin.length() < 2
              ? new NetcdfDataset(filenames[datasetIndex], task, dataFraction)
              : new NetcdfDataset(filenames[datasetIndex], task, dataBin);
    }
    if (!dataset->size()) {
      return next_dataset();
    }
    if (shuffled) {
      dataset->shuffle_sequences();
    }
    return false;
  }
  DataSequence *next_sequence() {
    bool finished = false;
    if (datasetIndex < 0 || seqIndex >= (int)last_index(dataset->sequences)) {
      finished = next_dataset();
    }
    if (finished) {
      clear_seq();
    } else {
      ++seqIndex;
      seq = dataset->sequences[seqIndex];
    }
    return seq;
  }
  DataSequence *start() {
    datasetIndex = -1;
    clear_seq();
    if (shuffled) {
      shuffle(filenames);
    }
    return next_sequence();
  }
  int size() const { return filenames.size(); }
  void print(ostream &out = cout) const {
    PRINT(numSequences, out);
    PRINT(numTimesteps, out);
    //		if(verbose)
    {
      out << "avg timesteps/seq = " << (real_t)numTimesteps /
                                           (real_t)numSequences << endl;
    }
    if (dataFraction != 1) {
      PRINT(dataFraction, out);
    }
    out << filenames.size() << " filenames" << endl;
    print_range(out, filenames, string("\n"));
    out << endl;
    //		if (verbose)
    {
      out << "inputSize = " << headers.front().inputSize << endl;
      out << "outputSize = " << headers.front().outputSize << endl;
      out << "numDims = " << headers.front().numDims << endl;
      PRINT(task, out);
      PRINT(shuffled, out);
      const vector<string> &targetLabels = headers.front().targetLabels;
      const vector<string> &inputLabels = headers.front().inputLabels;
      if (inputLabels.size()) {
        int totalHits = sum_right(inputLabelHits);
        out << inputLabels.size() << " input labels" << endl;
        LOOP(TIS t, enumerate(inputLabels)) {
          int hits = at(inputLabelHits, t.get<1>());
          out << t << " (" << hits << " = " << (hits * 100.0) / totalHits
              << "%)" << endl;
        }
      }
      if (targetLabels.size()) {
        int totalHits = sum_right(targetLabelCounts);
        out << targetLabels.size() << " target labels" << endl;
        LOOP(TIS t, enumerate(targetLabels)) {
          int hits = at(targetLabelCounts, t.get<1>());
          out << t << " (" << hits << " = " << (hits * 100.0) / totalHits
              << "%)" << endl;
        }
        PRINT(totalTargetStringLength, out);
      }
    }
  }
};

static ostream &operator<<(ostream &out, const DataList &dl) {
  dl.print(out);
  return out;
}

#endif
