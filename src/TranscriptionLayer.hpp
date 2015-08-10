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

#ifndef _INCLUDED_TranscriptionLayer_h
#define _INCLUDED_TranscriptionLayer_h

#include <boost/bimap.hpp>
#include "SoftmaxLayer.hpp"
#include "StringAlignment.hpp"

const Log<real_t> logOne(1);

struct TranscriptionLayer : public SoftmaxLayer, public NetworkOutput {
  // data
  ostream &out;
  SeqBuffer<Log<real_t> > forwardVariables;
  SeqBuffer<Log<real_t> > backwardVariables;
  int blank;
  int totalSegments;
  int totalTime;
  vector<Log<real_t> > dEdYTerms;
  vector<int> outputLabelSeq;
  bool confusionMatrix;

  // functions
  TranscriptionLayer(ostream &o, const string &name, const vector<string> &labs,
                     bool cm = false)
      : SoftmaxLayer(name, 1, make_target_labels(labs)), out(o),
        blank(index(targetLabels, "blank")), dEdYTerms(output_size()),
        confusionMatrix(cm) {
    criteria = list_of("ctcError")("labelError")("sequenceError");
    display(forwardVariables, "forwardVariables", &targetLabels);
    display(backwardVariables, "backwardVariables", &targetLabels);
  }
  virtual ~TranscriptionLayer() {}
  vector<string> make_target_labels(const vector<string> &labs) {
    vector<string> targetLabels = labs;
    targetLabels += "blank";
    return targetLabels;
  }
  integer_range<int> segment_range(int time, int totalSegs = -1) const {
    if (totalSegs < 0) {
      totalSegs = totalSegments;
    }
    int start = max(0, totalSegs - (2 * (totalTime - time)));
    int end = min(totalSegs, 2 * (time + 1));
    return span<int>(start, end);
  }
  vector<int> &path_to_string(const vector<int> &path) const {
    static vector<int> str;
    str.clear();
    int prevLabel = -1;
    LOOP(int label, path) {
      if (label != blank &&
          (str.empty() || label != str.back() || prevLabel == blank)) {
        str += label;
      }
      prevLabel = label;
    }
    return str;
  }
  vector<int> &best_label_seq() const {
    static vector<int> path;
    path.clear();
    LOOP(int i, span(outputActivations.seq_size())) {
      path += arg_max(outputActivations[i]);
    }
    return path_to_string(path);
  }
  virtual const Log<real_t> &prior_label_prob(int label) { return logOne; }
  real_t calculate_errors(const DataSequence &seq) {
    totalTime = outputActivations.seq_size();
    int requiredTime = seq.targetLabelSeq.size();
    int oldLabel = -1;
    LOOP(int label, seq.targetLabelSeq) {
      if (label == oldLabel) {
        ++requiredTime;
      }
      oldLabel = label;
    }
    if (totalTime < requiredTime) {
      out << "warning, seq " << seq.tag << " has requiredTime " << requiredTime
          << " > totalTime " << totalTime << endl;
      return 0;
    }
    totalSegments = (seq.targetLabelSeq.size() * 2) + 1;

    // calculate the forward variables
    forwardVariables.reshape_with_depth(list_of(totalTime), totalSegments, 0);
    forwardVariables.data[0] = logActivations.data[blank];
    if (totalSegments > 1) {
      forwardVariables.data[1] = logActivations.data[seq.targetLabelSeq[0]];
    }
    LOOP(int t, span(1, totalTime)) {
      View<Log<real_t> > logActs = logActivations[t];
      View<Log<real_t> > oldFvars = forwardVariables[t - 1];
      View<Log<real_t> > fvars = forwardVariables[t];
      LOOP(int s, segment_range(t)) {
        Log<real_t> fv;
        // s odd (label output)
        if (s & 1) {
          int labelIndex = s / 2;
          int labelNum = seq.targetLabelSeq[labelIndex];
          fv = oldFvars[s] + oldFvars[s - 1];
          if (s > 1 && (labelNum != seq.targetLabelSeq[labelIndex - 1])) {
            fv += oldFvars[s - 2];
          }
          fv *= logActs[labelNum] * prior_label_prob(labelIndex);
        }
        // s even (blank output)
        else {
          fv = oldFvars[s];
          if (s) {
            fv += oldFvars[s - 1];
          }
          fv *= logActs[blank];
        }
        fvars[s] = fv;
      }
    }
    View<Log<real_t> > lastFvs = forwardVariables[totalTime - 1];
    Log<real_t> logProb = lastFvs.back();
    if (totalSegments > 1) {
      logProb += nth_last(lastFvs, 2);
    }
    check(logProb.log() <= 0, "sequence\n" + str(seq) + "has log probability " +
                                  str(logProb.log()));

    // calculate the backward variables
    backwardVariables.reshape_with_depth(list_of(totalTime), totalSegments,
                                         Log<real_t>());
    View<Log<real_t> > lastBvs = backwardVariables[totalTime - 1];
    lastBvs.back() = 1;
    if (totalSegments > 1) {
      nth_last(lastBvs, 2) = 1;
    }
    // LOOP over time, calculating backward variables recursively
    LOOP_BACK(int t, span(totalTime - 1)) {
      View<Log<real_t> > oldLogActs = logActivations[t + 1];
      View<Log<real_t> > oldBvars = backwardVariables[t + 1];
      View<Log<real_t> > bvars = backwardVariables[t];
      LOOP(int s, segment_range(t)) {
        Log<real_t> bv;

        // s odd (label output)
        if (s & 1) {
          int labelIndex = s / 2;
          int labelNum = seq.targetLabelSeq[labelIndex];
          bv = (oldBvars[s] * oldLogActs[labelNum] *
                prior_label_prob(labelIndex)) +
               (oldBvars[s + 1] * oldLogActs[blank]);
          if (s < (totalSegments - 2)) {
            int nextLabelNum = seq.targetLabelSeq[labelIndex + 1];
            if (labelNum != nextLabelNum) {
              bv += (oldBvars[s + 2] * oldLogActs[nextLabelNum] *
                     prior_label_prob(labelIndex + 1));
            }
          }
        }

        // s even (blank output)
        else {
          bv = oldBvars[s] * oldLogActs[blank];
          if (s < (totalSegments - 1)) {
            bv += (oldBvars[s + 1] * oldLogActs[seq.targetLabelSeq[s / 2]] *
                   prior_label_prob(s / 2));
          }
        }
        bvars[s] = bv;
      }
    }
    // inject the training errors
    LOOP(int time, span(totalTime)) {
      fill(dEdYTerms, Log<real_t>(0));
      View<Log<real_t> > fvars = forwardVariables[time];
      View<Log<real_t> > bvars = backwardVariables[time];
      LOOP(int s, span(totalSegments)) {
        // k = blank for even s, target label for odd s
        int k = (s & 1) ? seq.targetLabelSeq[s / 2] : blank;
        dEdYTerms[k] += (fvars[s] * bvars[s]);
      }
      LOOP(TDLL t, zip(outputErrors[time], dEdYTerms, logActivations[time])) {
        t.get<0>() = -((t.get<1>() / (logProb * t.get<2>())).exp());
      }
    }
    // calculate the aligment errors
    outputLabelSeq = best_label_seq();
    StringAlignment<vector<int>, vector<int> > alignment(
        seq.targetLabelSeq, outputLabelSeq, verbose);
    real_t labelError = alignment.distance;
    real_t substitutions = alignment.substitutions;
    real_t deletions = alignment.deletions;
    real_t insertions = alignment.insertions;
    real_t seqError = labelError ? 1 : 0;
    real_t ctcError = -logProb.log();

    // store errors in map
    int normFactor = seq.targetLabelSeq.size();
    normFactors["labelError"] = normFactor;
    normFactors["substitutions"] = normFactor;
    normFactors["deletions"] = normFactor;
    normFactors["insertions"] = normFactor;
    errorMap.clear();
    ERR(labelError);
    ERR(seqError);
    ERR(substitutions);
    ERR(deletions);
    ERR(insertions);
    ERR(ctcError);
    if (confusionMatrix) {
      LOOP(const PII & p, alignment.delsMap) {
        errorMap["_" + targetLabels[p.first] + "_deletions"] +=
            (real_t)p.second / normFactor;
      }
      LOOP(const PII & p, alignment.insMap) {
        errorMap["_" + targetLabels[p.first] + "_insertions"] +=
            (real_t)p.second / normFactor;
      }
      typedef pair<int, map<int, int> > subsPair;
      LOOP(const subsPair & p, alignment.subsMap) {
        int refIndex = p.first;
        errorMap["_" + targetLabels[refIndex] + "_substitutions"] +=
            (real_t)sum_right(p.second) / normFactor;
        LOOP(const PII & p2, p.second) {
          errorMap
              ["_" + targetLabels[refIndex] + "->" + targetLabels[p2.first]] +=
              (real_t)p2.second / normFactor;
        }
      }
    }
    if (verbose) {
      out << "target label sequence (length " << seq.targetLabelSeq.size()
          << "):" << endl << label_seq_to_str(seq.targetLabelSeq, targetLabels)
          << endl;
      out << "output label sequence (length " << outputLabelSeq.size()
          << "):" << endl << label_seq_to_str(outputLabelSeq, targetLabels)
          << endl;
    }
    return ctcError;
  }
};

#endif
