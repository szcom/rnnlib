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

#include "fromstd.hpp"
#include "DataExporter.hpp"
void dumpState() {
  std::string filename("aborted.save");
  ofstream fout(filename.c_str());
  if (fout.is_open()) {
    cout << "saving to " << filename << endl;
    fout << DataExportHandler::instance();
    fout << endl << "rrrrrrrrrrrrr" << endl;
    DataExportHandler::instance().display("aborted");
  }
}
void DataExportHandler::save(ostream &out) const {
  LOOP(const PSPDE & exp, dataExporters) { out << *(exp.second); }
}
void DataExportHandler::load(ConfigFile &conf, ostream &out) {
  LOOP(PSPDE & exp, dataExporters) {
    if (!exp.second->load(conf, out)) {
      out << " for '" << exp.first << "' in config file " << conf.filename
          << ", exiting" << endl;
      exit(0);
    }
  }
}
void DataExportHandler::display(const string &path) const {
  LOOP(const PSPDE & exp, dataExporters) {
    LOOP(const PSPV & val, exp.second->displayVals) {
      string filename = path + exp.first + "_" + val.first;
      ofstream out(filename.c_str());
      check(out.is_open(),
            "couldn't open display file " + filename + " for writing");
      out << *(val.second);
    }
  }
}
