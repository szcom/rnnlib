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

#include <memory>
#include <boost/iostreams/tee.hpp>
#include <boost/iostreams/stream.hpp>
#include "Helpers.hpp"
#include "MultilayerNet.hpp"
#include "NetcdfDataset.hpp"
#include "GradientCheck.hpp"
#include "WeightContainer.hpp"
#include "SteepestDescent.hpp"
#include "Rprop.hpp"
#include "Trainer.hpp"
#include "MixtureOutputLayer.hpp"
#include "Rmsprop.hpp"
#include "VerticalNet.hpp"

typedef boost::iostreams::tee_device<ostream, ofstream> TeeDev;
typedef boost::iostreams::stream<TeeDev> TeeStream;

bool runningGradTest = false;
bool verbose = false;
ostream &COUT = std::cout;

vector<string> validDatasets = list_of<string>("train")("test")("val");
template <typename V, typename V1> void addVectors(V &a, const V1 &b) {
#ifdef FLOAT_REALS
  cblas_saxpy
#else
  cblas_daxpy
#endif
      (a.size(), 1, b.data(), 1, a.data(), 1);
#ifdef FLOAT_REALS
  cblas_sscal
#else
  cblas_dscal
#endif
      (a.size(), 0.5, a.data(), 1);
}
int main(int argc, char *argv[]) {
  //    rmsPropTest();
  if (argc < 3) {
    cout << "usage rnnsynth [config_options] config_file" << endl;
    cout << "config_options syntax: --<variable_name>=<variable_value>" << endl;
    cout << "whitespace not allowed in variable names or values" << endl;
    cout << "all config_file variables overwritten by config_options" << endl;
    cout << "setting <variable_value> = \"\" removes the variable from the "
            "config" << endl;
    cout << "repeated variables overwritten by last specified" << endl;
    exit(0);
  }
  ConfigFile conf(argv[argc - 2]);
  ConfigFile conf_1(argv[argc - 1]);
  bool autosave = false;
  string configFilename;
#ifdef FAST_LOGISTIC
  Logistic::fill_lookup();
#endif
  string task = conf.get<string>("task");
  CHECK_STRICT(task == "prediction", "must have prediction task");
  if (task == "prediction" && conf.get<int>("predictionSteps", 1) == 1) {
    task = conf.set_val<string>("task", "window-prediction");
  }
  verbose = true;
  ostream &out = cout;
  string dataset = conf.get<string>("dataset", "train");
  check(in(validDatasets, dataset),
        dataset + " given as 'dataset' parameter in config file '" +
            configFilename + "'\nmust be one of '" + str(validDatasets) + "'");
  string dataFileString = dataset + "File";

  vector<string> dataFiles = conf.get_list<string>(dataFileString);
  int dataFileNum = conf.get<int>("dataFileNum", 0);
  check(dataFiles.size() > dataFileNum,
        "no " + ordinal(dataFileNum) + " file in size " +
            str(dataFiles.size()) + " file list " + dataFileString + " in " +
            configFilename);
  string datafile = dataFiles[dataFileNum];
  DataHeader header(datafile, task, 1);

  PRINT(task, out);

  std::unique_ptr<Mdrnn> net;
  net.reset(new VerticalNet(out, conf, header));

  // build weight container after net is created
  WeightContainer &wc = WeightContainer::instance();
  wc.build();
  Rmsprop optimiser("weight_optimiser", out, wc.weights, wc.derivatives);
  int numWeights = WeightContainer::instance().weights.size();
  out << "loading dynamic data from " << conf.filename << endl;
  out << "number of weights to load " << numWeights << endl;
  DataExportHandler::instance().load(conf, out);
  Vector<real_t> weights = wc.weights;
  Vector<real_t> derivatives = wc.derivatives;
  Vector<real_t> deltas = optimiser.deltas;
  Vector<real_t> n = optimiser.n;
  Vector<real_t> g = optimiser.g;
  out << "loading dynamic data from " << conf_1.filename << endl;
  DataExportHandler::instance().load(conf_1, out);
  addVectors(wc.weights, weights);
  addVectors(wc.derivatives, derivatives);
  addVectors(optimiser.deltas, deltas);
  addVectors(optimiser.n, n);
  addVectors(optimiser.g, g);
  const std::string filename("agg.save");
  ofstream fout(filename.c_str());
  if (fout.is_open()) {
    out << "saving to " << filename << endl;
    fout << DataExportHandler::instance();
  } else {
    out << "WARNING unable to save to file " << filename << endl;
  }

  return 0;
}
