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

#ifndef rnnlib_xcode_MixtureOutputLayer_hpp
#define rnnlib_xcode_MixtureOutputLayer_hpp
#include "SoftmaxLayer.hpp"
#include "Random.hpp"
#include "BivariateNorm.h"
#include <boost/math/constants/constants.hpp>

struct MixtureOutputLayer : public NetworkOutput, public FlatLayer {
  static const size_t numMixElements = 6; /* pi, std(2), mean(2), corr */
  static const size_t offPi = 0;
  static const size_t offDevX = 1;
  static const size_t offDevY = 2;
  static const size_t offMuX = 3;
  static const size_t offMuY = 4;
  static const size_t offCorr = 5;
  static const size_t offTargetX = 0;
  static const size_t offTargetY = 1;
  static const size_t offTargetE = 2;

  ostream &out;
  int numMixtures;
  size_t offE;

  SeqBuffer<real_t> outputVariables;
  SeqBuffer<Log<real_t> > logActivationsWeights;
  real_t sampleBias;

  MixtureOutputLayer(ostream &o, const string &name, size_t numMix,
                     real_t sb = 0)
      : FlatLayer(name, 1,
                  numMix * numMixElements + 1), // +1 is for end of stroke flag
        out(o),
        numMixtures(numMix), offE(numMix * numMixElements),
        outputVariables(numMix * numMixElements + 1),
        logActivationsWeights(numMix), sampleBias(sb) {
    criteria += "loss";
  }
  void set_sample_bias(real_t b) {
    sampleBias = b;
    out << "sample bias:" << sampleBias << endl;
  }
  void start_sequence() {
    FlatLayer::start_sequence();
    outputVariables.reshape(outputActivations);
    logActivationsWeights.reshape(this->inputActivations);
  }

  void feed_forward(const vector<int> &coords) {

    View<real_t> inputMixtureCoeff =
        inputActivations[coords].slice(0, numMixtures);
    View<real_t> inputSigmaXY =
        inputActivations[coords].slice(numMixtures, 3 * numMixtures);
    View<real_t> paramMixtureCoeff =
        outputVariables[coords].slice(0, numMixtures);
    View<real_t> paramSigmaXY =
        outputVariables[coords].slice(numMixtures, 3 * numMixtures);

    // accent most probable mixture for biased sampling
    range_multiply_val(inputMixtureCoeff, 1.0 + sampleBias);
    // centre inputs on 0 for safer exponentiation for mixture weight
    real_t offset = max(inputMixtureCoeff);
    range_minus_val(inputMixtureCoeff, offset);
    // reduce variance for biased sampling
    range_minus_val(inputSigmaXY, sampleBias);
    LOOP(TDL t, zip(inputMixtureCoeff, logActivationsWeights[coords])) {
      t.get<1>() = Log<real_t>(t.get<0>(), true);
    }
    real_t *act = this->inputActivations[coords].begin();
    real_t *var = this->outputVariables[coords].begin();
    // deviations and weight
    transform(inputSigmaXY, paramSigmaXY, Log<real_t>::safe_exp);
    Log<real_t> sumPi = sum(logActivationsWeights[coords]);
    range_divide_val(logActivationsWeights[coords], sumPi);
    transform(logActivationsWeights[coords], paramMixtureCoeff,
              mem_fun_ref(&Log<real_t>::exp));

    act += numMixtures * 3;
    var += numMixtures * 3;
    // means
    copy(act, act + numMixtures * 2, var);
    act += numMixtures * 2;
    var += numMixtures * 2;
    // rho
    std::transform(act, act + numMixtures, var, Tanh::fn);

    act += numMixtures;
    var += numMixtures;
    // e
    *var = Logistic::fn(-(*act));

    bound_range(paramSigmaXY, almostZero, realMax);
  }

  void feed_back(const vector<int> &coords) {}
  real_t calculate_errors(const DataSequence &seq) {
    Log<real_t> loss(0, true);

    LOOP(int pt, span(seq.inputs.seq_size() - 1)) {
      const real_t *target_t = seq.targetPatterns[pt].begin();

      MixtureView<real_t> outpVars(outputVariables[pt], numMixElements,
                                   numMixtures);
      View<Log<real_t> > logWeights = logActivationsWeights[pt];

      Vector<real_t> C(numMixtures), Z(numMixtures), Z_mul_C(numMixtures);
      Vector<Log<real_t> > logRespUnnormed(numMixtures), logResp(numMixtures);
      FOR(i, numMixtures) {
        Z[i] = BivariateNorm::Z(
            target_t[offTargetX], target_t[offTargetY], outpVars.get(i, offMuX),
            outpVars.get(i, offMuY), outpVars.get(i, offDevX),
            outpVars.get(i, offDevY), outpVars.get(i, offCorr));
        real_t c =
            (1.0 - outpVars.get(i, offCorr)) * (1 + outpVars.get(i, offCorr));
        c = std::max(c, almostZero);
        C[i] = 1.0 / c;

        Z_mul_C[i] = -Z[i] * 0.5 * C[i];
      }
      real_t offset = max(Z_mul_C);
      range_minus_val(Z_mul_C, offset);
      FOR(i, numMixtures) {
        logRespUnnormed[i] =
            logWeights[i] * Log<real_t>(sqrt(C[i])) *
            Log<real_t>(Z_mul_C[i], true) /
            Log<real_t>(2 * boost::math::constants::pi<real_t>() *
                        outpVars.get(i, offDevX) * outpVars.get(i, offDevY));
      }

      Log<real_t> logRespSum = sum(logRespUnnormed);
      range_divide_val(logResp, logRespUnnormed, logRespSum);

      real_t eosProb = target_t[offTargetE] ? outputVariables[pt][offE]
                                            : 1. - outputVariables[pt][offE];

      loss *= logRespSum * Log<real_t>(offset, true);
      loss *= Log<real_t>(eosProb);

      partial_derivs(pt, target_t, logResp, C, Z);
      if (!runningGradTest) {
        bound_range(inputErrors[pt], -100.0, 100.0);
      }
    }
    errorMap["loss"] = -loss.log();
    return -loss.log();
  }
  void partial_derivs(int pt, const real_t *target_t,
                      Vector<Log<real_t> > &resp, const Vector<real_t> &C,
                      Vector<real_t> &Z) {

    View<real_t> intpuErrors_t = inputErrors[pt];
    View<real_t> outputVariables_t = outputVariables[pt];

    intpuErrors_t[offE] = target_t[offTargetE] - outputVariables_t[offE];
    View<Log<real_t> > logWeights_t = logActivationsWeights[pt];

    MixtureView<real_t> errs(intpuErrors_t, numMixElements, numMixtures);
    MixtureView<real_t> vars(outputVariables_t, numMixElements, numMixtures);
    FOR(i, numMixtures) {

      errs.get(i, offPi) =
          (LogExpression<real_t>(logWeights_t[i]) - resp[i]).val();

      real_t dX = target_t[offTargetX] - vars.get(i, offMuX);
      real_t dY = target_t[offTargetY] - vars.get(i, offMuY);
      real_t sX = vars.get(i, offDevX);
      real_t sY = vars.get(i, offDevY);
      real_t rho = vars.get(i, offCorr);
      real_t npi = -resp[i].exp();
      //////  dL/dMuX
      errs.get(i, offMuX) = npi * C[i] / sX * (dX / sX - rho * dY / sY);
      //////  dL/dMuY
      errs.get(i, offMuY) = npi * C[i] / sY * (dY / sY - rho * dX / sX);

      //////  dL/dSx
      errs.get(i, offDevX) = errs.get(i, offMuX) * dX + resp[i].exp();

      //////  dL/dSy
      errs.get(i, offDevY) = errs.get(i, offMuY) * dY + resp[i].exp();

      ///// dL/dRho
      errs.get(i, offCorr) =
          npi * (dX * dY / (sY * sX) + rho * (1 - C[i] * Z[i]));
    }
  }
  PDD sample_mixture(int pt, int n) {
    View<real_t> outputVariables_t = outputVariables[pt];
    MixtureView<real_t> vars(outputVariables_t, numMixElements, numMixtures);

    return Random::binormal(vars.get(n, offDevX), vars.get(n, offMuX),
                            vars.get(n, offDevY), vars.get(n, offMuY),
                            vars.get(n, offCorr));
  }
  real_t mixture_weight(int pt, int n) {
    View<real_t> outputVariables_t = outputVariables[pt];
    MixtureView<real_t> vars(outputVariables_t, numMixElements, numMixtures);
    return vars.get(n, offPi);
  }
  virtual Vector<real_t> sample(int pt) {
    Vector<real_t> rv(3);
    double x = 0;
    double y = 0;
    real_t p = Random::uniform();
    real_t ub = 0;
    FOR(i, numMixtures) {
      ub += mixture_weight(pt, i);
      if (p < ub || ub == 1 || i + 1 == numMixtures) {
        PDD pdd = sample_mixture(pt, i);
        x = pdd.first;
        y = pdd.second;
        break;
      }
    }
    View<real_t> vars = outputVariables[pt];
    real_t e = Random::bernoulli(vars[offE]) ? 1.0 : 0.0;
    rv[0] = (real_t)x;
    rv[1] = (real_t)y;
    rv[2] = e;
    return rv;
  }
};

struct MixtureSamplingLayer : public MixtureOutputLayer {
  Layer *inputLayer;
  int primeLen;
  int lastEosTs;
  MixtureSamplingLayer(ostream &o, Layer *input, const string &name,
                       size_t numMix, real_t sb = 0)
      : MixtureOutputLayer(o, name, numMix, sb), inputLayer(input),
        primeLen(-1), lastEosTs(0) {}
  void feed_forward(const vector<int> &coords) {
    MixtureOutputLayer::feed_forward(coords);
    sample(coords[0]);
  }

  virtual Vector<real_t> sample(int pt) {
    Vector<real_t> rv = MixtureOutputLayer::sample(pt);
    if (pt < primeLen) {
      return rv;
    }
    if (inputLayer->outputActivations.in_range({ pt + 1 })) {
      if (rv[2] == 1.) {
        lastEosTs = pt;
      }
      const int maxStepsWithoutEndOfStroke = 100;
      if (pt - lastEosTs > maxStepsWithoutEndOfStroke) {
        rv[2] = 1;
        lastEosTs = pt;
      }
      inputLayer->outputActivations[pt + 1] = rv;
    }
    return rv;
  }
  void set_prime_length(int l) {
    primeLen = l;
    lastEosTs = 0;
  }
};

#endif
