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

#ifndef _INCLUDED_ClassificationLayer_h
#define _INCLUDED_ClassificationLayer_h

#include "SoftmaxLayer.hpp"

struct ClassificationLayer : public NetworkOutput {
  // data
  ostream &out;
  Vector<string> labels;
  SeqBuffer<int> targets;
  vector<vector<int> > confusionMatrix;
  vector<int> numErrorsByClass;
  vector<int> numTargetsByClass;
  vector<int> outputs;

  // functions
  ClassificationLayer(ostream &o, const vector<string> &labs)
      : out(o), labels(labs), targets(labels.size()),
        confusionMatrix(labels.size()), numErrorsByClass(labels.size()),
        numTargetsByClass(labels.size()) {
    LOOP(vector<int> & v, confusionMatrix) { v.resize(labels.size()); }
    criteria = list_of("crossEntropyError")("classificationError");
  }
  virtual int output_class(int pt) const = 0;
  virtual real_t class_prob(int pt, int index) const = 0;
  virtual real_t set_error(int pt, int targetClass) = 0;
  real_t calculate_errors(const DataSequence &seq) {
    LOOP(vector<int> & v, confusionMatrix) { fill(v, 0); }
    outputs.clear();
    targets.reshape(seq.targetClasses.seq_shape(), 0);
    real_t crossEntropyError = 0;
    LOOP(int pt, span(seq.targetClasses.seq_size())) {
      int outputClass = output_class(pt);
      outputs += outputClass;
      int targetClass = seq.targetClasses[pt].front();
      if (targetClass >= 0) {
        View<int> targs = targets[pt];
        targs[targetClass] = 1;
        crossEntropyError -= set_error(pt, targetClass);
        ++confusionMatrix[targetClass][outputClass];
      }
    }
    errorMap.clear();
    LOOP(int i, indices(confusionMatrix)) {
      vector<int> &v = confusionMatrix[i];
      numTargetsByClass[i] = sum(v);
      numErrorsByClass[i] = numTargetsByClass[i] - v[i];
    }
    real_t numTargets = sum(numTargetsByClass);
    if (numTargets) {
      errorMap["crossEntropyError"] = crossEntropyError;
      errorMap["classificationError"] = sum(numErrorsByClass) / numTargets;
      LOOP(int i, indices(confusionMatrix)) {
        if (numTargetsByClass[i]) {
          errorMap["_" + labels[i]] = numErrorsByClass[i] / numTargets;
          if (verbose && (confusionMatrix.size() > 2)) {
            vector<int> &v = confusionMatrix[i];
            LOOP(int j, indices(v)) {
              if (j != i && v[j]) {
                errorMap["_" + labels[i] + "->" + labels[j]] =
                    v[j] / numTargets;
              }
            }
          }
        }
      }
    }
    return crossEntropyError;
  }
};

struct MulticlassClassificationLayer : public ClassificationLayer,
                                       public SoftmaxLayer {
  // functions
  MulticlassClassificationLayer(ostream &out, const string &name,
                                size_t numSeqDims, const vector<string> &labels)
      : ClassificationLayer(out, labels),
        SoftmaxLayer(name, numSeqDims, labels) {
    // display(targets, "targets", labels);
  }
  int output_class(int pt) const { return arg_max(outputActivations[pt]); }
  real_t class_prob(int pt, int index) const {
    return max(realMin, outputActivations[pt][index]);
  }
  real_t set_error(int pt, int targetClass) {
    real_t targetProb = class_prob(pt, targetClass);
    View<real_t> errs = outputErrors[pt];
    errs[targetClass] = -(1 / targetProb);
    return log(targetProb);
  }
};

struct BinaryClassificationLayer : public ClassificationLayer,
                                   public NeuronLayer<Logistic> {
  BinaryClassificationLayer(ostream &out, const string &name, size_t numSeqDims,
                            const vector<string> &labels)
      : ClassificationLayer(out, labels),
        NeuronLayer<Logistic>(name, numSeqDims, 1) {
    // display(targets, "targets", labels);
  }
  int output_class(int pt) const {
    return (outputActivations[pt][0] > 0.5 ? 1 : 0);
  }
  real_t class_prob(int pt, int index) const {
    real_t act = max(realMin, outputActivations[pt][0]);
    return (index == 1 ? act : 1 - act);
  }
  real_t set_error(int pt, int targetClass) {
    real_t targetProb = class_prob(pt, targetClass);
    ((View<real_t>)outputErrors[pt])[0] =
        (targetClass ? -(1 / targetProb) : (1 / targetProb));
    return log(targetProb);
  }
};

ClassificationLayer *make_classification_layer(ostream &out, const string &name,
                                               size_t numSeqDims,
                                               const vector<string> &labels) {
  assert(labels.size() >= 2);
  if (labels.size() == 2) {
    return new BinaryClassificationLayer(out, name, numSeqDims, labels);
  }
  return new MulticlassClassificationLayer(out, name, numSeqDims, labels);
}

#endif
