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

#ifndef _INCLUDED_Trainer_h
#define _INCLUDED_Trainer_h
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <chrono>
#include <memory>
#include "Mdrnn.hpp"
#include "Optimiser.hpp"
#include "NetcdfDataset.hpp"
#include "Helpers.hpp"
#include "ConfigFile.hpp"
#include "DatasetErrors.hpp"
#include "Random.hpp"
#include "Rmsprop.hpp"

;
extern bool verbose;
#ifdef OP_TRACKING
extern unsigned long long matrixOps;
#endif

struct Trainer : public DataExporter {
  // data
  ostream &out;
  Mdrnn *net;
  std::unique_ptr<Optimiser> optimiser;
  ConfigFile &config;
  Vector<real_t> &wts;
  Vector<real_t> &derivs;
  int epoch;
  Vector<string> &criteria;
  map<string, real_t> &netErrors;
  const map<string, real_t> &netNormFactors;
  vector<real_t> distortedWeights;
  string task;
  real_t dataFraction;
  string dataBin;
  int seqsPerWeightUpdate;
  bool batchLearn;
  DataList trainFiles;
  DataList testFiles;
  DataList valFiles;
  DatasetErrors trainErrors;
  DatasetErrors testErrors;
  DatasetErrors valErrors;
  real_t inputNoiseDev;
  real_t weightDistortion;
  bool testDistortions;
  real_t l1;
  real_t l2;
  real_t invTrainSeqs;

  // MDL parameters
  bool mdl;
  real_t mdlWeight;
  real_t mdlInitStdDev;
  Vector<real_t> mdlStdDevs;
  Vector<real_t> mdlStdDevDerivs;
  Vector<real_t> weightCosts;
  int mdlSamples;
  bool mdlSymmetricSampling;
  Vector<real_t> mdlSeqDerivs;
  Vector<real_t> mdlOldDerivs;
  real_t mdlPriorMean;
  real_t mdlPriorStdDev;
  real_t mdlPriorVariance;
  std::unique_ptr<Optimiser> mdlOptimiser;
  Vector<prob_t> mdlMlErrors;
  map<string, real_t> mdlSeqErrors;

  // functions
  Trainer(ostream &o, Mdrnn *n, ConfigFile &conf,
          const string &name = "trainer")
      : DataExporter(name), out(o), net(n), config(conf),
        wts(WeightContainer::instance().weights),
        derivs(WeightContainer::instance().derivatives), epoch(0),
        criteria(net->criteria), netErrors(net->errors),
        netNormFactors(net->normFactors), task(config.get<string>("task")),
        dataFraction(config.get<real_t>("dataFraction", 1)),
        dataBin(config.get<string>("dataBin", ".")),
        seqsPerWeightUpdate(config.get<int>("seqsPerWeightUpdate", 1)),
        batchLearn(config.get<bool>(
            "batchLearn",
            (config.get<string>("optimiser", "steepest") == "rprop") &&
                (seqsPerWeightUpdate == 1))),
        trainFiles(config.get_list<string>("trainFile"), task, !batchLearn,
                   dataFraction, dataBin),
        testFiles(config.get_list<string>("testFile"), task, false,
                  dataFraction, dataBin),
        valFiles(config.get_list<string>("valFile"), task, false, dataFraction,
                 dataBin),
        inputNoiseDev(config.get<real_t>("inputNoiseDev", 0)),
        weightDistortion(config.get<real_t>(
            "weightDistortion",
            0)), // std deviation of distortions to add to wts during training
        testDistortions(conf.get<bool>("testDistortions", false)),
        l1(config.get<real_t>("l1", 0)), l2(config.get<real_t>("l2", 0)),
        invTrainSeqs(trainFiles.numSequences ? 1.0 / trainFiles.numSequences
                                             : 0),

        // MDL parameters
        mdl(config.get<bool>("mdl", false)),
        mdlWeight(mdl ? config.get<real_t>("mdlWeight", 1) : 1),
        mdlInitStdDev(mdl ? config.get<real_t>("mdlInitStdDev", 0.075) : 0),
        mdlStdDevs(mdl ? wts.size() : 0, mdlInitStdDev),
        mdlStdDevDerivs(mdlStdDevs.size(), 0),
        weightCosts(mdlStdDevs.size(), 0),
        mdlSamples(mdl ? config.get<int>("mdlSamples", 1) : 0),
        mdlSymmetricSampling(
            mdl ? config.get<bool>("mdlSymmetricSampling", false) : false),
        mdlPriorMean(0), mdlPriorStdDev(0), mdlPriorVariance(0) {
    string optType = config.get<string>("optimiser", "steepest");
    string optName = "weight_optimiser";
    real_t learnRate = config.get<real_t>("learnRate", 1e-4);
    real_t momentum = config.get<real_t>("momentum", 0.9);
    if (optType == "rprop") {
      optimiser.reset(new Rprop(optName, out, wts, derivs));
    } else if (optType == "rproponline") {
      optimiser.reset(new Rprop(optName, out, wts, derivs, true));
    } else if (optType == "rmsprop") {
      optimiser.reset(new Rmsprop(optName, out, wts, derivs));
    } else {
      optimiser.reset(
          new SteepestDescent(optName, out, wts, derivs, learnRate, momentum));
    }
    if (mdl) {
      string mdlOptType = config.get<string>("mdlOptimiser", optType);
      string mdlOptName = "mdl_dev_optimiser";
      if (mdlOptType == "rmsprop") {
        mdlOptimiser.reset(
            new Rmsprop(mdlOptName, out, mdlStdDevs, mdlStdDevDerivs));
      } else if (mdlOptType == "rprop") {
        mdlOptimiser.reset(
            new Rprop(mdlOptName, out, mdlStdDevs, mdlStdDevDerivs));
      } else {
        mdlOptimiser.reset(
            new SteepestDescent(mdlOptName, out, mdlStdDevs, mdlStdDevDerivs,
                                config.get<real_t>("mdlLearnRate", learnRate),
                                config.get<real_t>("mdlMomentum", momentum)));
      }
      SAVE(mdlPriorMean);
      SAVE(mdlPriorVariance);
      WeightContainer::instance().save_by_conns(mdlStdDevs, "_mdl_devs");
      WeightContainer::instance().save_by_conns(weightCosts,
                                                "_mdl_weight_costs");
    }
    SAVE(epoch);
  }
  real_t mdl_mean(int i) { return wts[i]; }
  real_t mdl_std_dev(int i) { return abs(mdlStdDevs[i]); }
  real_t mdl_variance(int i) { return squared(mdlStdDevs[i]); }
  void mdl_calculate_prior_params() {
    real_t W = wts.size();
    mdlPriorMean = mean(wts);
    mdlPriorVariance = 0;
    FOR(i, wts.size()) {
      mdlPriorVariance += mdl_variance(i) + squared(mdl_mean(i) - mdlPriorMean);
    }
    mdlPriorVariance /= W;
    mdlPriorVariance = std::max(almostZero, mdlPriorVariance);
    mdlPriorStdDev = sqrt(mdlPriorVariance);
  }
  real_t mdl_evaluate() {
    mdl_calculate_prior_params();
    real_t weightNats = 0;
    FOR(i, wts.size()) {
      weightNats += KL_normal(mdl_mean(i), mdl_variance(i), mdlPriorMean,
                              mdlPriorVariance);
      ;
    }
    return weightNats * mdlWeight;
  }
  void mdl_differentiate(real_t scaleFactor = 1) {
    mdl_calculate_prior_params();
    LOOP(int i, indices(derivs)) {
      real_t stdDev = std::max(almostZero, mdl_std_dev(i));
      real_t mean = mdl_mean(i);
      derivs[i] += (scaleFactor * (mean - mdlPriorMean)) / mdlPriorVariance;
      mdlStdDevDerivs[i] +=
          scaleFactor * ((stdDev / mdlPriorVariance) - (1 / stdDev));
    }
  }
  void mdl_sample_weights(int sampleNum) {
    if (mdlSymmetricSampling && (sampleNum & 1)) {
      LOOP(int i, indices(wts)) {
        distortedWeights[i] = (2 * wts[i]) - distortedWeights[i];
      }
      distortedWeights.swap(wts);
    } else {
      distortedWeights = wts;
      LOOP(int i, indices(distortedWeights)) {
        perturb_weight(wts[i], mdl_std_dev(i));
      }
    }
  }
  real_t mdl_ml_error() {
    real_t error = 0;
    LOOP(prob_t err, mdlMlErrors) { error -= err.log(); }
    error /= mdlMlErrors.size();
    mdlSeqErrors /= mdlSamples;
    netErrors[criteria.front()] = error;
    netErrors = mdlSeqErrors;
    return error;
  }
  real_t weight_cost(int i) {
    return KL_normal(mdl_mean(i), mdl_variance(i), mdlPriorMean,
                     mdlPriorVariance);
  }
  void mdl_print_stats() {
    mdl_calculate_prior_params();
    PRINT(minmax(wts), out);
    PRINT(std_dev(wts), out);
    PRINT(minmax(mdlStdDevs), out);
    PRINT(mean(mdlStdDevs), out);
    PRINT(std_dev(mdlStdDevs), out);
    PRINT(mdlPriorMean, out);
    PRINT(mdlPriorStdDev, out);
    PRINT(mdlPriorVariance, out);

    // store weight costs
    LOOP(int i, indices(wts)) { weightCosts[i] = weight_cost(i); }
    PRINT(minmax(weightCosts), out);
    PRINT(mean(weightCosts), out);
    PRINT(std_dev(weightCosts), out);
  }
  real_t evaluate(const DataSequence *seq) {
    real_t error = 0;
    if (mdl) {
      mdlSeqErrors.clear();
      mdlMlErrors.clear();
      FOR(s, mdlSamples) {
        seq = apply_distortions(seq);
        mdl_sample_weights(s);
        net->calculate_errors(*seq);
        mdlMlErrors += prob_t(-netErrors[criteria.front()], true);
        mdlSeqErrors += netErrors;
        distortedWeights.swap(wts);
      }
      error = mdl_ml_error();
    } else {
      seq = apply_distortions(seq);
      error = net->calculate_errors(*seq);
      revert_distortions();
    }
    return error;
  }
  real_t differentiate(const DataSequence *seq) {
    real_t error = 0;
    if (mdl) {
      mdlSeqErrors.clear();
      mdlMlErrors.clear();
      if ((mdlSamples > 1) || (seqsPerWeightUpdate > 1)) {
        flood(mdlSeqDerivs, derivs.size(), 0);
        mdlOldDerivs = derivs;
      }
      FOR(s, mdlSamples) {
        if (verbose) {
          out << "sample " << s << endl;
        }
        seq = apply_distortions(seq);
        mdl_sample_weights(s);
        if ((mdlSamples > 1) || (seqsPerWeightUpdate > 1)) {
          fill(derivs, 0);
        }
        net->train(*seq);
        mdlSeqErrors += netErrors;
        real_t sampleError = netErrors[criteria.front()];
        if (verbose) {
          PRINT(sampleError, out);
        }
        mdlMlErrors += prob_t(-sampleError, true);
        FOR(i, wts.size()) {
          real_t curvature = squared(derivs[i]);
          real_t stdDev = mdl_std_dev(i);
          if ((mdlSamples == 1) && (seqsPerWeightUpdate == 1)) {
            mdlStdDevDerivs[i] += (curvature * stdDev);
          } else {
            mdlStdDevDerivs[i] += (curvature * stdDev) / mdlSamples;
            mdlSeqDerivs[i] += derivs[i];
          }
        }
        distortedWeights.swap(wts);
      }
      if (mdlSamples > 1) {
        derivs = mdlOldDerivs;
        range_divide_val(mdlSeqDerivs, mdlSamples);
        range_plus_equals(derivs, mdlSeqDerivs);
      }
      error = mdl_ml_error();
    } else {
      seq = apply_distortions(seq);
      error = net->train(*seq);
      revert_distortions();
    }
    return error;
  }
  void regularise(real_t scaleFactor = 1) {
    if (mdl) {
      mdl_differentiate(scaleFactor);
    } else {
      if (l1) {
        FOR(i, derivs.size()) { derivs[i] += scaleFactor * l1 * sign(wts[i]); }
        real_t l1Error = nats_to_bits(l1 * abs_sum(wts));
        trainErrors.add_error("l1ErrorBits", l1Error);
      }
      if (l2) {
        FOR(i, derivs.size()) { derivs[i] += scaleFactor * l2 * wts[i]; }
        real_t l2Error = nats_to_bits(0.5 * l2 * inner_product(wts, wts));
        trainErrors.add_error("l2ErrorBits", l2Error);
      }
    }
  }
  void calculate_compression_errors(DatasetErrors &errors) {
    if (mdl && in(errors.errors, criteria.front())) {
      real_t errorBits = nats_to_bits(errors.errors[criteria.front()]);
      real_t totalBits = errorBits;
      if ((mdl) && ((&errors) == (&trainErrors))) {
        real_t weightBits = nats_to_bits(mdl_evaluate());
        totalBits += weightBits;
        errors.errors["weightBits"] = weightBits;
        errors.errors["bitsPerWeight"] = weightBits / wts.size();
        errors.errors["totalBits"] = totalBits;
        if (errors.normFactors.find("bitsPerSymbol") !=
            errors.normFactors.end()) {
          errors.errors["totalBitsPerSymbol"] =
              totalBits / errors.normFactors["bitsPerSymbol"];
        }
      }
      errors.errors["errorBits"] = errorBits;
    }
  }
  void set_dropout_mode(bool is_training) {
    int layersWithDropout = config.get<int>("dropout", 0);
    for (int l_i = layersWithDropout; l_i > 0; l_i--) {
      net->hiddenLayers[net->hiddenLayers.size() - l_i]
          ->set_dropout_mode(is_training);
    }
  }
  void train(const string &savename) {
    check(trainFiles.size(), "no training files loaded");
    int totalEpochs = config.get<int>("totalEpochs", -1);
    int maxTestsNoBest = config.get<int>("maxTestsNoBest", 20);
    int layersWithDropout = config.get<int>("dropout", 0);

    PRINT(epoch, out);
    if (totalEpochs >= 0) {
      PRINT(totalEpochs, out);
    }
    if (savename != "") {
      PRINT(savename, out);
    }
    PRINT(batchLearn, out);
    PRINT(seqsPerWeightUpdate, out);
    PRINT(maxTestsNoBest, out);
    out << endl;
    print_datasets();
    print_distortions();
    prt_line(out);
    out << *optimiser;
    if (mdl) {
      if (!in(criteria, "weightBits")) {
        criteria += "weightBits";
      }
      if (!in(criteria, "totalBits")) {
        criteria += "totalBits";
      }
      prt_line(out);
      if (mdlOptimiser) {
        out << *mdlOptimiser;
      }
    }
    prt_line(out);
    out << endl;

    // init filenames
    string bestSaveRoot = savename + ".best";
    string lastSaveFile = savename + ".last.save";
    if (savename != "") {
      out << "autosave filename " << lastSaveFile << endl;
      out << "best save filename root " << bestSaveRoot << endl << endl;
    }
    config.warn_unused(out);

// init LOOP variables
#ifndef OP_TRACKING
    int numWeights = wts.size();
#endif
    map<string, pair<int, DatasetErrors> > bestTestErrors;
    map<string, pair<int, DatasetErrors> > bestValErrors;
    map<string, pair<int, DatasetErrors> > bestTrainErrors;
    int testsSinceBest = 0;

    // LOOP through training data until done
    out << "training..." << endl;
    auto t = std::chrono::high_resolution_clock::now();
    int initEpoch = epoch;
    int seqsSinceWeightUpdate = 0;
    bool stoppingCriteriaReached = false;
    while (!stoppingCriteriaReached &&
           (epoch < totalEpochs || totalEpochs < 0)) {
      auto epochT = std::chrono::high_resolution_clock::now();
      trainErrors.clear();
#ifdef OP_TRACKING
      matrixOps = 0;
#endif
      // print MDL stats
      if (mdl) {
        out << endl << "MDL stats:" << endl;
        mdl_print_stats();
      }

      // run through one epoch, collecting errors and updating weights
      set_dropout_mode(true);

      for (const DataSequence *seq = trainFiles.start(); seq;
           seq = trainFiles.next_sequence()) {
        if (verbose) {
          out << "data sequence:" << endl;
          out << "file = " << trainFiles.dataset->filename << endl;
          out << "index = " << trainFiles.seqIndex << endl;
          out << *seq;
        }
        differentiate(seq);
        if (!(batchLearn) && (++seqsSinceWeightUpdate >= seqsPerWeightUpdate)) {
          regularise(invTrainSeqs * seqsSinceWeightUpdate * mdlWeight);
          update_weights();
          seqsSinceWeightUpdate = 0;
        }
        trainErrors.add_seq_errors(netErrors, netNormFactors);
        if (verbose) {
          net->print_output_shape(out);
          out << "errors:" << endl;
          out << netErrors;
          if (mdl) {
            prt_line(out);
            mdl_print_stats();
            out << "bitsPerWeight = " << nats_to_bits(mdl_evaluate()) /
                                             wts.size() << endl;
            prt_line(out);
          }
          out << endl;
        }
      }
      if (batchLearn) {
        regularise(mdlWeight);
        update_weights();
      }
      calculate_compression_errors(trainErrors);
      trainErrors.normalise();

      // print out epoch data
      auto epochEndT = std::chrono::high_resolution_clock::now();
      real_t epochSeconds = std::chrono::duration_cast<std::chrono::seconds>(
          epochEndT - epochT).count();
      out << endl << "epoch " << epoch << " took ";
      print_time(epochSeconds, out);
      real_t itsPerSec = (real_t)trainFiles.numTimesteps / epochSeconds;
      out << " (";
      print_time(epochSeconds / (real_t)trainFiles.numSequences, out, true);
      out << "/seq, " << itsPerSec << " its/sec, ";
#ifdef OP_TRACKING
      real_t mWtOpsPerSec = matrixOps / (epochSeconds * 1e6);
      out << mWtOpsPerSec << " MwtOps/sec)" << endl;
#else
      real_t mWtItsPerSec = (itsPerSec * numWeights) / 1e6;
      out << mWtItsPerSec << " MwtIts/sec)" << endl;
#endif
      // print running train errrors
      prt_line(out);
      out << "train errors (running):" << endl;
      out << trainErrors;
      prt_line(out);

      // calculate error test, if required
      set_dropout_mode(false);
      if (valFiles.size()) {
        out << "validation errors:" << endl;
        out << calculate_errors(valFiles, valErrors);
        prt_line(out);
      }
      if (testFiles.size()) {
        out << "test errors:" << endl;
        out << calculate_errors(testFiles, testErrors);
        prt_line(out);
      }

      // update epoch BEFORE saves (so training continues one epoch on)
      ++epoch;
      if (savename != "") {
        save_data(lastSaveFile, config);
      }
      DatasetErrors currentErrors = valFiles.size() ? valErrors : trainErrors;
      map<string, pair<int, DatasetErrors> > &bestErrors =
          valFiles.size() ? bestValErrors : bestTrainErrors;
      if (check_for_best(currentErrors, bestErrors, epoch)) {
        LOOP(const PSPIDE & p, bestErrors) {
          if (p.second.first == epoch) {
            const string &s = p.first;
            out << "best network (" << s << ")" << endl;
            if (valFiles.size()) {
              bestTrainErrors[s] = make_pair(epoch, trainErrors);
            }
            if (testFiles.size()) {
              bestTestErrors[s] = make_pair(epoch, testErrors);
            }
            if (savename != "") {
              string saveFile = bestSaveRoot + "_" + s + ".save";
              save_data(saveFile, config);
            }
            testsSinceBest = 0;
          }
        }
      }
      // check if training is finished
      else if (maxTestsNoBest > 0 && (++testsSinceBest > maxTestsNoBest)) {
        out << testsSinceBest << " error tests without best, ending training"
            << endl;
        stoppingCriteriaReached = true;
      }
    }

    // autosave, if required
    if (savename != "") {
      save_data(lastSaveFile, config);
    }

    // print out overall stats
    auto tEnd = std::chrono::high_resolution_clock::now();
    real_t seconds =
        std::chrono::duration_cast<std::chrono::milliseconds>(tEnd - t).count();
    out << endl << "training finished, " << epoch << " epochs in total" << endl;
    out << epoch - initEpoch << " epochs in ";
    print_time(seconds, out);
    out << "(this session)" << endl << endl;
    print_best_errors("train", bestTrainErrors);
    out << endl;
    if (valFiles.size()) {
      print_best_errors("validation", bestValErrors);
      out << endl;
    }
    if (testFiles.size()) {
      print_best_errors("test", bestTestErrors);
      out << endl;
    }
  }
  bool print_distortions() {
    bool distortions = false;
    if (inputNoiseDev != 0) {
      out << "adding noise to input data, std dev " << inputNoiseDev << endl
          << endl;
      distortions = true;
    }
    if (weightDistortion) {
      out << "adding  noise to weights every sequence, std dev "
          << weightDistortion << endl << endl;
      distortions = true;
    }
    if (mdl) {
      prt_line(out);
      out << "MDL training" << endl;
      PRINT(mdlWeight, out);
      PRINT(mdlInitStdDev, out);
      PRINT(mdlSamples, out);
      PRINT(mdlSymmetricSampling, out);
      distortions = true;
    }
    if (l1) {
      prt_line(out);
      PRINT(l1, out);
      distortions = true;
    }
    if (l2) {
      prt_line(out);
      PRINT(l2, out);
      distortions = true;
    }
    return distortions;
  }
  const DataSequence *apply_distortions(const DataSequence *seq) {
    if (inputNoiseDev) {
      seq = add_input_noise(seq);
    }
    if (weightDistortion) {
      distortedWeights = wts;
      perturb_weights(distortedWeights, weightDistortion, true);
      distortedWeights.swap(wts);
    }
    return seq;
  }
  const void revert_distortions() {
    if (weightDistortion) {
      distortedWeights.swap(wts);
    }
  }
  DataSequence *add_input_noise(const DataSequence *seq) {
    static DataSequence noisySeq;
    noisySeq = *seq;
    LOOP(real_t & f, noisySeq.inputs.data) {
      f += Random::normal(inputNoiseDev);
    }
    return &noisySeq;
  }
  void print_datasets() const {
    if (trainFiles.size()) {
      out << "training data:" << endl << trainFiles;
    }
    if (valFiles.size()) {
      prt_line(out);
      out << "validation data:" << endl << valFiles;
    }
    if (testFiles.size()) {
      prt_line(out);
      out << "test data:" << endl << testFiles;
    }
    out << endl;
  }
  void save_data(const string &filename, ConfigFile &conf) {
    ofstream fout(filename.c_str());
    if (fout.is_open()) {
      out << "saving to " << filename << endl;
      config.set_val<bool>("loadWeights", true);
      fout << config << DataExportHandler::instance();
    } else {
      out << "WARNING trainer unable to save to file " << filename << endl;
    }
  }
  void update_weights() {
    if (mdl) {
      optimiser->update_weights();
      if (mdlOptimiser) {
        mdlOptimiser->update_weights();
      }
      LOOP(real_t & d, mdlStdDevs) { d = abs(d); }
    } else {
      optimiser->update_weights();
    }
    reset_derivs();
  }
  void reset_derivs() {
    fill(derivs, 0);
    fill(mdlStdDevDerivs, 0);
  }
  bool check_for_best(const DatasetErrors &currentErrors,
                      map<string, pair<int, DatasetErrors> > &bestErrors,
                      int epoch) {
    bool newBest = false;
    LOOP(const PSD & p, currentErrors.errors) {
      const string &errName = p.first;
      real_t err = p.second;
      if (in(criteria, errName) &&
          (!in(bestErrors, errName) ||
           !(in(bestErrors[errName].second.errors, errName)) ||
           err < bestErrors[errName].second.errors[errName])) {
        newBest = true;
        bestErrors[errName] = make_pair(epoch, currentErrors);
      }
    }
    return newBest;
  }
  void print_best_errors(
      const string &name,
      const map<string, pair<int, DatasetErrors> > &bestErrors) const {
    out << name << " set errors for best networks" << endl;
    LOOP(const PSPIDE & p, bestErrors) {
      prt_line(out);
      out << "epoch " << p.second.first << " (" << p.first << ")" << endl
          << p.second.second;
    }
  }
  DatasetErrors &calculate_errors(DataList &data, DatasetErrors &errors) {
    errors.clear();
    for (DataSequence *seq = data.start(); seq; seq = data.next_sequence()) {
      if (verbose) {
        out << "data sequence:" << endl;
        out << "file = " << data.dataset->filename << endl;
        out << "index = " << data.seqIndex << endl;
        out << *seq;
      }
      net->calculate_errors(*seq);
      errors.add_seq_errors(netErrors, netNormFactors);
      if (verbose) {
        net->print_output_shape(out);
        out << "errors:" << endl;
        out << netErrors << endl;
      }
    }
    errors.normalise();
    return errors;
  }
  void calculate_all_errors() {
    print_datasets();
    boost::timer t;
    if (trainFiles.numSequences) {
      out << "calculating train errors..." << endl;
      calculate_errors(trainFiles, trainErrors);
    }
    if (valFiles.numSequences) {
      out << "calculating validation errors..." << endl;
      calculate_errors(valFiles, valErrors);
    }
    if (testFiles.numSequences) {
      out << "calculating test errors..." << endl;
      calculate_errors(testFiles, testErrors);
    }
    real_t seconds = t.elapsed();
    if (trainFiles.numSequences) {
      out << "train errors:" << endl;
      out << trainErrors;
    }
    if (valFiles.numSequences) {
      prt_line(out);
      out << "validation errors:" << endl;
      out << valErrors;
    }
    if (testFiles.numSequences) {
      prt_line(out);
      out << "test errors:" << endl;
      out << testErrors;
    }
    int numSequences = trainFiles.numSequences + testFiles.numSequences +
                       valFiles.numSequences;
    if (numSequences) {
      prt_line(out);
      out << numSequences << " sequences tested in ";
      print_time(seconds, out);
      out << endl;
      out << "average ";
      print_time(seconds / numSequences, out);
      out << " per sequence" << endl;
    } else {
      out << "WARNING: all data sets empty" << endl;
    }
  }
};

#endif
