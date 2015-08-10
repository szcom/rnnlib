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
template <class T>
bool rough_eq(T lhs, T rhs, T epsilon = 1e-10) // operator==
{
  return fabs(lhs - rhs) < epsilon;
}

int main(int argc, char *argv[]) {
  //    rmsPropTest();
  if (argc < 2) {
    cout << "usage rnnsynth [config_options] config_file" << endl;
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
  bool autosave = conf.get<bool>("autosave", false);
  string configFilename;
  string task = conf.get<string>("task");
  CHECK_STRICT(task == "prediction", "must have prediction task");
  if (task == "prediction" && conf.get<int>("predictionSteps", 1) == 1) {
    task = conf.set_val<string>("task", "window-prediction");
  }
  bool display = conf.get<bool>("display", false);
  vector<int> jacobianCoords = conf.get_list<int>("jacobianCoords");
  bool gradCheck = conf.get<bool>("gradCheck", false);
  verbose = conf.get<bool>("verbose", false);
  int primeId = conf.get<int>("primeId", 0);
  primeId = bound(primeId, 0, 5);
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
  std::unique_ptr<DataSequence> testSeq;
  if (gradCheck) {
    NetcdfDataset *data = new NetcdfDataset(datafile, task, 0);
    testSeq.reset(new DataSequence((*data)[0]));
    delete data;
  }

  std::unique_ptr<Mdrnn> net;
  net.reset(new VerticalNet(out, conf, header, !gradCheck));

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

  if (gradCheck) {
    real_t initWeightRange = conf.get<real_t>("initWeightRange", 0.1);
    int numRandWts = wc.randomise(initWeightRange);

    check(sizeof(real_t) == sizeof(double),
          "real_t must be double when checking gradient");
    out << "data header:" << endl << header << endl;
    out << *testSeq;
    testSeq->targetSentence = "dummy";
    prt_line(out);
    GradientCheck(out, net.get(), *testSeq, conf.get<int>("sigFigs", 6),
                  conf.get<real_t>("pert", 1e-5),
                  conf.get<bool>("verbose", false),
                  conf.get<bool>("breakOnError", true));
    conf.warn_unused(out);
    return 0;
  }

  CHECK_STRICT(conf.get<bool>("loadWeights", false),
               "configure with loadWeights=true");

  string dumpPath = "";
  string logname = "";
  if (display) {
    dumpPath = conf.get<string>("dumpPath", "synthdump");
    logname = dumpPath + "log";
  }

  if (conf.get<bool>("loadWeights", false)) {
    out << "loading dynamic data from " << conf.filename << endl;
    DataExportHandler::instance().load(conf, out);
    out << "epoch = " << trainer.epoch << endl << endl;

    NetcdfDataset *data = new NetcdfDataset(datafile, task);
    while (true) {
      std::vector<int> charWindowSizes = conf.get_list<int>("charWindowSize");
      std::string sentence;
      std::getline(std::cin, sentence);
      if (sentence.find("conf_conf_conf:") == 0) {
        std::stringstream cs(sentence.substr(sentence.find(":") + 1));
        real_t sampleBias;
        cs >> primeId >> sampleBias;
        primeId = bound(primeId, 0, 5);
        int primeIdSeqIndex[] = { 0, 9903, 650, 5467, 6890, 5496 };
        primeId = primeIdSeqIndex[primeId];
        sampleBias = bound(sampleBias, (real_t)0, (real_t)10);
        net->set_sample_bias(sampleBias);
        continue;
      }
      if (charWindowSizes.size()) {
        CHECK_STRICT(!sentence.empty(), "add --sentence=<what>");
      }
      out << "sentence:" << sentence << endl;
      DataSequence s(3);
      const int MAX_SAMPLES = 3000;
      vector<real_t> shape(1, MAX_SAMPLES);
      s.inputs.reshape(shape, 0);
      s.inputs.data[0] = -0.1968395;
      s.inputs.data[1] = -0.003023848;
      s.targetSentence = sentence;
      int i = 0;
      net->set_prime_length(-1);
      if (primeId) {
        std::unique_ptr<DataSequence> ps;
        ps.reset(new DataSequence((*data)[primeId]));
        copy(ps->inputs.data, s.inputs.data);
        s.targetSentence = ps->targetSentence + " " + sentence;
        out << "primed sentence:" << s.targetSentence << endl;
        net->set_prime_length(ps->num_timesteps() - 1);
        i = ps->num_timesteps() - 1;
      }
      out << "generating samples from network" << endl;
      net->feed_forward(s);
      for (; !net->end_of_target_string(i) && i < MAX_SAMPLES; ++i) {
        Layer *input = net->get_input_layer();
        View<real_t> sam = input->out_acts({ i });
        out << "Sample " << sam << endl;
      }
      out << "End of sentence" << endl;
      if (display) {
        out << "data header:" << endl << header << endl;
        out << "displaying sequence " << s.targetSentence << endl;
        out << s;
        out << "output path: " << endl << dumpPath << endl;
        // net->train(s);
        net->print_output_shape(out);
        out << "errors:" << endl << net->errors;
        DataExportHandler::instance().display(dumpPath);
        conf.warn_unused(out);
      }
    }
  }
  return 0;
}
