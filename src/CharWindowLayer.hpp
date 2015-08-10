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

#ifndef rnnlib_CharWindowLayer_hpp
#define rnnlib_CharWindowLayer_hpp
#include <string>
#include <vector>
#include <functional>
#include "Layer.hpp"
#include "Log.hpp"
#include "MultiArray.hpp"
#include "Matrix.hpp"

struct CharWindowLayer : public Layer {
  static const size_t kNumMixElements = 3; /* a,b,k */
  static const size_t kOffAlphaWeight = 0;
  static const size_t kOffBetaWidth = 1;
  static const size_t kOffKLocation = 2;
  static const size_t kNumChars = 58;
  const std::string
  kAlphabet; // "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890,.!:'
             // ?";
  SeqBuffer<Log<real_t> > mixtureParameters;
  size_t numMixtures;
  std::string sentence;
  MultiArray<real_t> oneHotSentence;
  Vector<LogExpression<real_t> > lastDlDk;
  Vector<Log<real_t> > lastK;

  ostream &out;

  CharWindowLayer(ostream &o, const string &name, size_t numMix)
      : Layer(name, 1, numMix * kNumMixElements, kNumChars),
        kAlphabet("aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ ,!?'"),
        mixtureParameters(numMix * kNumMixElements), numMixtures(numMix),
        lastDlDk(numMix), lastK(numMix), out(o) {
    sentence = "dummy";
    CHECK_STRICT(kNumChars == kAlphabet.size() + 1, "check size of alphabet");
  }
  void set_sentence(const std::string &txt) { sentence = txt; }
  void start_sequence() {
    Layer::start_sequence();
    mixtureParameters.reshape(inputActivations);
    oneHotSentence.reshape(
        vector<size_t>({ sentence.size(), kAlphabet.size() + 1 }), 0);
    FOR(i, sentence.size()) {
      size_t pos = kAlphabet.find(sentence[i]);
      if (pos == std::string::npos) {
        pos = kAlphabet.size();
      }
      oneHotSentence.get({ i, static_cast<int>(pos) }) = real_t(1.0);
    }
    fill(lastDlDk, LogExpression<real_t>());
    fill(lastK, 0);
  }

  virtual void feed_forward(const std::vector<int> &coords) {
    MixtureView<real_t> mixtureInputView(inputActivations[coords],
                                         kNumMixElements, numMixtures);

    MixtureView<Log<real_t> > mixtureParametersView(
        mixtureParameters[coords], kNumMixElements, numMixtures);
    LOOP(TDL t,
         zip(this->inputActivations[coords], mixtureParameters[coords])) {
      t.get<1>() = Log<real_t>(t.get<0>(), true);
    }
    FOR(k, numMixtures) {
      mixtureParametersView.get(k, kOffKLocation) =
          lastK[k] + mixtureParametersView.get(k, kOffKLocation);
      lastK[k] = mixtureParametersView.get(k, kOffKLocation);
    }

    Vector<Log<real_t> > conv(sentence.size());
    get_chars_likelyhood(conv.slice(), coords);

    View<real_t> outView = outputActivations[coords];
    Log<real_t> *in = conv.slice().begin();
    Log<real_t> *inEnd = conv.slice().end();
    Vector<Log<real_t> > logOuts(kNumChars);
    Log<real_t> *outBegin = logOuts.slice().begin();
    Log<real_t> *outEnd = logOuts.slice().end();
    real_t *M = oneHotSentence.data.slice().begin();
    for (; in != inEnd; ++in) {
      Log<real_t> input = *in;
      for (Log<real_t> *out = outBegin; out != outEnd; ++out, ++M) {
        if (*M == 1) {
          *out += input;
        }
      }
    }
    transform(logOuts.slice(), outView, mem_fun_ref(&Log<real_t>::exp));
  }
  virtual bool eol_reached(const vector<int> &coords) {
    Vector<Log<real_t> > conv(sentence.size() + 1);
    get_chars_likelyhood(conv.slice(), coords);
    int maxU = arg_max(conv.slice());
    return maxU == sentence.size();
  }
  virtual void feed_back(const vector<int> &coords) {
    Vector<LogExpression<real_t> > epsilon(sentence.size());
    View<real_t> outputErr = outputErrors[coords];
    LogExpression<real_t> *outEnd = epsilon.slice().end();
    LogExpression<real_t> *out = epsilon.slice().begin();
    real_t *M = oneHotSentence.data.slice().begin();
    real_t *inBegin = outputErr.begin();
    real_t *inEnd = outputErr.end();
    for (; out != outEnd; ++out) {
      LogExpression<real_t> sum;
      for (const real_t *in = inBegin; in != inEnd; ++in, ++M) {
        if (*M == 1) {
          sum = sum + (*in);
        }
      }
      out->add(sum);
    }

    MixtureView<Log<real_t> > mixtureParametersView(
        mixtureParameters[coords], kNumMixElements, numMixtures);
    MixtureView<real_t> mixtureInputErrorView(inputErrors[coords],
                                              kNumMixElements, numMixtures);
    MixtureView<real_t> mixtureInputView(inputActivations[coords],
                                         kNumMixElements, numMixtures);

    FOR(i, numMixtures) {
      LogExpression<real_t> alphaSum;
      LogExpression<real_t> betaSum;
      LogExpression<real_t> dLdk;
      Log<real_t> k = mixtureParametersView.get(i, kOffKLocation);
      Log<real_t> alpha = mixtureParametersView.get(i, kOffAlphaWeight);
      FOR(u, static_cast<int>(sentence.size())) {
        LogExpression<real_t> k_minus_u(k);
        k_minus_u = k_minus_u - real_t(u + 1);
        LogExpression<real_t> u_minus_k(u + 1.);
        u_minus_k = u_minus_k - k;

        Log<real_t> ebku(exp_bku(coords, i, u));
        alphaSum = alphaSum + epsilon[u] * ebku;
        betaSum = betaSum + epsilon[u] * alpha * ebku * squared(k_minus_u);
        dLdk = dLdk + epsilon[u] * alpha * ebku * u_minus_k;
      }
      alphaSum = alphaSum * alpha;
      mixtureInputErrorView.get(i, kOffAlphaWeight) = alphaSum.val();
      betaSum = betaSum * mixtureParametersView.get(i, kOffBetaWidth);
      mixtureInputErrorView.get(i, kOffBetaWidth) = -betaSum.val();
      // k
      dLdk = dLdk * real_t(2) * mixtureParametersView.get(i, kOffBetaWidth) +
             lastDlDk[i];
      lastDlDk[i] = dLdk;
      dLdk = dLdk * Log<real_t>(mixtureInputView.get(i, kOffKLocation), true);
      mixtureInputErrorView.get(i, kOffKLocation) = dLdk.val();
    }
    if (!runningGradTest) {
      bound_range(inputErrors[coords], -10.0, 10.0);
    }
  }

private:
  void get_chars_likelyhood(View<Log<real_t> > res,
                            const std::vector<int> &coords) {
    MixtureView<Log<real_t> > mixtureParametersView(
        mixtureParameters[coords], kNumMixElements, numMixtures);

    FOR(u, res.size()) {
      Log<real_t> charLikelyhood;
      FOR(k, numMixtures) {
        charLikelyhood += mixtureParametersView.get(k, kOffAlphaWeight) *
                          exp_bku(coords, k, u);
      }
      res[u] = charLikelyhood;
    }
  }
  Log<real_t> exp_bku(const std::vector<int> &coords, int k, int u) {
    MixtureView<Log<real_t> > mixtureParametersView(
        mixtureParameters[coords], kNumMixElements, numMixtures);
    real_t bku =
        -mixtureParametersView.get(k, kOffBetaWidth).exp() *
        squared(mixtureParametersView.get(k, kOffKLocation).exp() - u - 1);
    return Log<real_t>(bku, true);
  }
};

#endif
