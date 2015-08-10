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

#ifndef _INCLUDED_MultilayerNet_h
#define _INCLUDED_MultilayerNet_h

#include "Mdrnn.hpp"
#include "ClassificationLayer.hpp"
#include "TranscriptionLayer.hpp"
#include "MixtureOutputLayer.hpp"

struct MultilayerNet : public Mdrnn {
  MixtureSamplingLayer *samplingLayer;
  // functions
  MultilayerNet(ostream &out, ConfigFile &conf, const DataHeader &data,
                bool samplingOutput = false)
      : Mdrnn(out, conf, data), samplingLayer(0) {
    string task = conf.get<string>("task");
    vector<int> hiddenSizes = conf.get_list<int>("hiddenSize");
    vector<int> charWindowSizes = conf.get_list<int>("charWindowSize");
    assert(hiddenSizes.size());
    Vector<string> hiddenTypes =
        conf.get_list<string>("hiddenType", "lstm", hiddenSizes.size());
    Vector<Vector<size_t> > hiddenBlocks =
        conf.get_array<size_t>("hiddenBlock");
    assert(hiddenBlocks.size() < hiddenSizes.size());
    Vector<int> subsampleSizes = conf.get_list<int>("subsampleSize");
    assert(subsampleSizes.size() < hiddenSizes.size());
    string subsampleType = conf.get<string>("subsampleType", "tanh");
    bool subsampleBias = conf.get<bool>("subsampleBias", false);
    Vector<bool> recurrent =
        conf.get_list<bool>("recurrent", true, hiddenSizes.size());
    Layer *input = this->get_input_layer();
    bool wantGatherLayer = task != "prediction";
    bool hasSkipConnections = conf.get<bool>("skipConnections", true);
    LOOP(int i, indices(hiddenSizes)) {
      string level_suffix = int_to_sortable_string(i, hiddenSizes.size());
      this->add_hidden_level(hiddenTypes.at(i), hiddenSizes.at(i),
                             recurrent.at(i), "hidden_" + level_suffix);
      this->connect_to_hidden_level(input, i, input == get_input_layer() &&
                                                  !samplingOutput);
      vector<Layer *> blocks;
      if (i < hiddenBlocks.size()) {
        LOOP(Layer * l, hiddenLevels[i]) {
          blocks += this->add_layer(new BlockLayer(l, hiddenBlocks.at(i)));
        }
      }
      vector<Layer *> &topLayers = blocks.size() ? blocks : hiddenLevels[i];
      if (i < subsampleSizes.size()) {
        input = this->add_layer(
            subsampleType, "subsample_" + level_suffix, subsampleSizes.at(i),
            empty_list_of<int>().repeat(this->num_seq_dims(), 1), subsampleBias,
            false);
        LOOP(Layer * l, topLayers) { this->connect_layers(l, input); }
      } else if (i < last_index(hiddenSizes)) {
        input = wantGatherLayer ? this->add_layer(new GatherLayer(
                                      "gather_" + level_suffix, topLayers))
                                : hiddenLayers.back();
      }
    }
    conf.set_val("inputSize", inputLayer->output_size());
    if (data.targetLabels.size()) {
      string labelDelimiters(",.;:|+&_~*%$#^=-<>/?{}[]()");
      LOOP(char c, labelDelimiters) {
        bool goodDelim = true;
        LOOP(const string & s, data.targetLabels) {
          if (in(s, c)) {
            goodDelim = false;
            break;
          }
        }
        if (goodDelim) {
          stringstream ss;
          print_range(ss, data.targetLabels, c);
          conf.set_val("targetLabels", ss.str());
          conf.set_val("labelDelimiter", c);
          break;
        }
      }
    }
    string outputName = "output";
    Layer *output = 0;
    size_t outSeqDims = (in(task, "sequence_") ? 0 : num_seq_dims());
    if (in(task, "classification")) {
      output = add_output_layer(make_classification_layer(
          out, outputName, outSeqDims, data.targetLabels));
    } else if (task == "transcription") {
      check(this->num_seq_dims(), "cannot perform transcription wth 0D net");
      output = add_output_layer(
          new TranscriptionLayer(out, outputName, data.targetLabels,
                                 conf.get<bool>("confusionMatrix", false)));
      if (this->num_seq_dims() > 1) {
        output =
            this->collapse_layer(hiddenLayers.back(), output, list_of(true));
      }
    } else if (task == "prediction") {
      real_t sampleBias = conf.get<real_t>("sampleBias", 0);
      output =
          samplingOutput == false
              ? add_output_layer(new MixtureOutputLayer(
                    out, outputName, conf.get<int>("mixtures", 20), sampleBias))
              : add_output_layer(samplingLayer = new MixtureSamplingLayer(
                                     out, get_input_layer(), outputName,
                                     conf.get<int>("mixtures", 20),
                                     sampleBias));
    } else {
      check(false, "unknown task '" + task + "'");
    }
    if (this->num_seq_dims() && in(task, "sequence_")) {
      output = this->collapse_layer(hiddenLayers.back(), output);
    }
    if (hiddenSizes.size()) {
      connect_from_hidden_level(last_index(hiddenLevels), output);
    }
    if (hasSkipConnections) {
      add_skip_connections(output, get_input_layer(), hiddenSizes);
    }
    if (charWindowSizes.size()) {
      add_window(charWindowSizes[0]);
    }
  }
  void add_skip_connections(Layer *output, Layer *input,
                            const vector<int> &hiddenSizes) {
    LOOP(int i, indices(hiddenSizes)) {
      if (i > 0) {
        connect_to_hidden_level(input, i, false);
      }
      if (i < last_index(hiddenSizes)) {
        connect_from_hidden_level(i, output);
      }
    }
  }
  virtual void set_prime_length(int len) {
    samplingLayer->set_prime_length(len);
  }
  virtual void set_sample_bias(real_t b) { samplingLayer->set_sample_bias(b); }
};

#endif
