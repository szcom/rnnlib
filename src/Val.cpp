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
template <class T>
bool rough_eq(T lhs, T rhs, T epsilon = 1e-10) // operator==
{
  return fabs(lhs - rhs) < epsilon;
}

int main(int argc, char *argv[]) {
  //    rmsPropTest();
  if (argc < 2) {
    cout << "usage rnnval [config_options] config_file" << endl;
    cout << "config_options syntax: --<variable_name>=<variable_value>" << endl;
    cout << "whitespace not allowed in variable names or values" << endl;
    cout << "all config_file variables overwritten by config_options" << endl;
    cout << "setting <variable_value> = \"\" removes the variable from the "
            "config" << endl;
    cout << "repeated variables overwritten by last specified" << endl;
    exit(0);
  }
  ConfigFile conf(argv[argc - 1]);
  LOOP(int arg, span(1, argc - 1)) {
    vector<string> argument = split<string>(argv[arg], '=', 2);
    check(argument[0].substr(0, 2) == "--",
          "invalid option name " + argument[0]);
    string varName = argument[0].substr(2);
    if (argument.size() < 2 || argument[1] == "\"\"") {
      conf.remove(varName);
    } else {
      conf.set_val<string>(varName, argument[1], false);
    }
  }
  string configFilename;
  string task = conf.get<string>("task");
  CHECK_STRICT(task == "prediction", "must have prediction task");
  if (task == "prediction" && conf.get<int>("predictionSteps", 1) == 1) {
    task = conf.set_val<string>("task", "window-prediction");
  }
  bool display = conf.get<bool>("display", false);
  vector<int> jacobianCoords = conf.get_list<int>("jacobianCoords");
  verbose = conf.get<bool>("verbose", false);
  int displaySequence = conf.get<int>("sequence", 0);
  string dataset = conf.get<string>("dataset", "train");
  check(in(validDatasets, dataset),
        dataset + " given as 'dataset' parameter in config file '" +
            configFilename + "'\nmust be one of '" + str(validDatasets) + "'");
  string dataFileString = dataset + "File";
  ostream &out = cout;
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
  net.reset(new MultilayerNet(out, conf, header));

  // build weight container after net is created
  WeightContainer &wc = WeightContainer::instance();
  wc.build();
  int numWeights = WeightContainer::instance().weights.size();

  // build the network after the weight container
  net->build();

  // print out network
  out << endl << "network:" << endl;
  PRINT(task, out);
  out << *net;
  out << numWeights << " weights" << endl << endl;

  // create trainer
  Trainer trainer(out, net.get(), conf);
  out << "setting random seed to "
      << Random::set_seed(conf.get<unsigned long int>("randSeed", 0)) << endl
      << endl;
  CHECK_STRICT(conf.get<bool>("loadWeights", false),
               "configure with loadWeights=true");
  out << "loading dynamic data from " << conf.filename << endl;
  DataExportHandler::instance().load(conf, out);
  out << "epoch = " << trainer.epoch << endl << endl;
  real_t dataFraction(conf.get<real_t>("dataFraction", 1));
  string dataBin(conf.get<string>("dataBin", "."));
  DataList valFiles(conf.get_list<string>("valFile"), task, false, dataFraction,
                    dataBin);
  DatasetErrors valErrors;
  out << trainer.calculate_errors(valFiles, valErrors) << endl;
  out << "validation errors:" << valErrors.errors["loss"] << endl;
  return 0;
}
