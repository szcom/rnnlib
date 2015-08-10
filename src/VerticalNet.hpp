#ifndef _RNNLIB_VerticalNet_hpp
#define _RNNLIB_VerticalNet_hpp
#include "MultilayerNet.hpp"

struct VerticalNet : public MultilayerNet {
  VerticalNet(ostream &out, ConfigFile &conf, const DataHeader &data,
              bool samplingOutput = false)
      : MultilayerNet(out, conf, data, samplingOutput) {}

  virtual void feed_forward(const DataSequence &seq) {
    check(seq.inputs.size(), "empty inputs in sequence\n" + str(seq));
    errors.clear();
    inputLayer->copy_inputs(seq.inputs);
    if (charWindowLayer) {
      charWindowLayer->set_sentence(seq.targetSentence);
    }
    LOOP(int pt, span(seq.inputs.seq_size())) {
      LOOP(Layer * layer, hiddenLayers) { feed_forward_layer(layer, pt); }
      LOOP(Layer * layer, outputLayers) { feed_forward_layer(layer, pt); }
      if (charWindowLayer->eol_reached({ pt })) {
        return;
      }
    }
  }
  void feed_forward_layer(Layer *layer, int pt) {
    bool isFirstHidden = layer == hiddenLayers.front();
    if (pt == 0) {
      layer->start_sequence();
      if (isFirstHidden) {
        charWindowLayer->start_sequence();
      }
    }
    pair<CONN_IT, CONN_IT> connRange = connections.equal_range(layer);
    LOOP(PLC c, connRange) {
      c.second->feed_forward({ pt });
    }
    layer->feed_forward({ pt });
    if (isFirstHidden) {
      pair<CONN_IT, CONN_IT> windowConnRange =
          connections.equal_range(charWindowLayer);
      LOOP(PLC c, windowConnRange) {
        c.second->feed_forward({ pt });
      }
      charWindowLayer->feed_forward({ pt });
    }
  }
};
#endif