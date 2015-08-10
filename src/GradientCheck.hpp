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

#ifndef _INCLUDED_GradientCheck_h
#define _INCLUDED_GradientCheck_h

#include "WeightContainer.hpp"
#include "Mdrnn.hpp"

extern bool runningGradTest;

struct GradientCheck {
  // data
  ostream &out;
  map<string, bool> checked;
  Mdrnn *net;
  const DataSequence &seq;
  real_t perturbation;
  unsigned sigFigs;
  vector<real_t> &weights;
  vector<real_t> &derivs;
  bool verbose;
  bool breakOnError;
  multimap<string, tuple<string, string, int, int> > &conns;

  // functions
  GradientCheck(ostream &o, Mdrnn *n, const DataSequence &s, unsigned sf = 6,
                real_t pert = 1e-5, bool verb = false, bool csd = false,
                bool boe = true)
      : out(o), net(n), seq(s), perturbation(pert), sigFigs(sf),
        weights(WeightContainer::instance().weights),
        derivs(WeightContainer::instance().derivatives), verbose(verb),
        breakOnError(boe), conns(WeightContainer::instance().connections) {
    runningGradTest = true;
    PRINT(perturbation, out);
    PRINT(sigFigs, out);
    PRINT(verbose, out);
    PRINT(breakOnError, out);
    prt_line(out);
    out << "calculating algorithmic pds" << endl;
    net->train(seq);
    out << "checking against numeric pds" << endl;
    LOOP(Layer * l, net->outputLayers) {
      if (!check_layer(l->name)) {
        out << "GRADIENT CHECK FAILED!" << endl << endl;
        exit(0);
      }
    }
    LOOP_BACK(Layer * l, net->hiddenLayers) {
      if (!check_layer(l->name)) {
        out << "GRADIENT CHECK FAILED!" << endl << endl;
        exit(0);
      }
    }
    out << "GRADIENT CHECK SUCCESSFUL!" << endl;
    runningGradTest = false;
  }
  bool check_layer(const string &name) {
    bool retVal = true;
    if (!checked[name]) {
      checked[name] = true;
      pair<WC_CONN_IT, WC_CONN_IT> range = conns.equal_range(name);
      if (range.first != range.second) {
        prt_line(out);
        out << "checking layer " << name << endl;
        LOOP(const WC_CONN_PAIR & p, range) {
          if (!check_connection(p.second.get<1>(), p.second.get<2>(),
                                p.second.get<3>())) {
            retVal = false;
            if (breakOnError) {
              goto exit;
            }
          }
        }
        LOOP(const WC_CONN_PAIR & p, range) {
          if (!check_layer(p.second.get<0>())) {
            return false;
          }
        }
      }
    }
  exit:
    if (!breakOnError && retVal == false) {
      out << name << ": CHECK FAILED" << endl;
    }
    return retVal;
  }
  bool check_connection(const string &name, int begin, int end) {
    if (begin == end) {
      return true;
    }
    out << "checking connection " << name << endl;
    bool retVal = true;
    LOOP(int i, span(begin, end)) {
      // store original weight
      real_t oldWt = weights[i];

      // add positive perturbation and compute error
      weights[i] += perturbation;
      real_t plusErr = net->calculate_errors(seq);

      // add negative perturbation and compute error
      weights[i] = oldWt - perturbation;
      real_t minusErr = net->calculate_errors(seq);

      // store symmetric difference
      real_t numericDeriv = (plusErr - minusErr) / (2 * perturbation);

      // restore original weight
      weights[i] = oldWt;

      real_t algoDeriv = derivs[i];
      int index = i - begin;
      real_t threshold = pow(
          (real_t)10.0,
          max((real_t)0.0,
              (real_t)ceil(log10(min(fabs(algoDeriv), fabs(numericDeriv))))) -
              (int)sigFigs);
      real_t diff = fabs(numericDeriv - algoDeriv);
      bool wrong = (isnan(diff) || diff > threshold);
      if (verbose || wrong) {
        out << "weight " << index << " numeric deriv " << numericDeriv
            << " algorithmic deriv " << algoDeriv << endl;
      }
      if (wrong) {
        retVal = false;
        if (breakOnError) {
          break;
        }
      }
    }
    if (!breakOnError && retVal == false) {
      out << name << ": CHECK FAILED" << endl;
    }
    return retVal;
  }
};

#endif
