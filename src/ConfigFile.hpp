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

#ifndef _INCLUDED_ConfigFile_h
#define _INCLUDED_ConfigFile_h

#include <iostream>
#include <map>
#include <string>
#include <sstream>
#include <algorithm>
#include <iterator>
#include "Helpers.hpp"
#include "String.hpp"

;

struct ConfigFile {
  // data
  set<string> used;
  map<string, string> params;
  string filename;

  // functions
  ConfigFile(const string &fname, char readLineChar = '_') : filename(fname) {
    ifstream instream(filename.c_str());
    check(instream.is_open(),
          "could not open config file \"" + filename + "\"");
    string name;
    string val;
    while (instream >> name && instream >> val) {
      string line;
      getline(instream, line);
      if (name[0] != '#') {
        if (in(name, readLineChar) && line.size() > 1) {
          val += line;
        }
        params[name] = val;
      }
    }
  }
  bool contains(const string &name) const { return in(params, name); }
  bool remove(const string &name) {
    if (contains(name)) {
      params.erase(name);
      used.erase(name);
      return true;
    }
    return false;
  }
  template <class T>
  const T &set_val(const string &name, const T &val, bool valUsed = true) {
    stringstream ss;
    ss << boolalpha << val;
    params[name] = ss.str();
    if (valUsed) {
      used.insert(name);
    }
    return val;
  }
  template <class T> T get(const string &name, const T &defaultVal) {
    MSSI it = params.find(name);
    if (it == params.end()) {
      set_val<T>(name, defaultVal);
      return defaultVal;
    }
    return get<T>(name);
  }
  template <class T> T get(const string &name) {
    MSSCI it = params.find(name);
    check(it != params.end(),
          "param '" + name + "' not found in config file '" + filename);
    used.insert(name);
    return read<T>(it->second);
  }
  template <class T>
  Vector<T> get_list(const string &name, const char delim = ',') {
    Vector<T> vect;
    MSSCI it = params.find(name);
    if (it != params.end()) {
      vect = split_with_repeat<T>(it->second, delim);
      used.insert(name);
    }
    return vect;
  }
  template <class T>
  Vector<T> get_list(const string &name, const T &defaultVal, size_t length,
                     const char delim = ',') {
    Vector<T> vect = get_list<T>(name, delim);
    vect.resize(length, vect.size() == 1 ? vect.front() : defaultVal);
    used.insert(name);
    return vect;
  }
  template <class T>
  Vector<Vector<T> > get_array(const string &name, const char delim1 = ';',
                               const char delim2 = ',') {
    Vector<Vector<T> > array;
    MSSCI it = params.find(name);
    if (it != params.end()) {
      LOOP(const string & row, split<string>(it->second, delim1)) {
        array += split_with_repeat<T>(row, delim2);
      }
      used.insert(name);
    }
    return array;
  }
  template <class T>
  Vector<Vector<T> > get_array(const string &name, const string &defaultStr,
                               size_t length, const char delim1 = ';',
                               const char delim2 = ',') {
    Vector<Vector<T> > array = get_array<T>(name, delim1, delim2);
    array.resize(length, array.size() == 1
                             ? array.front()
                             : split_with_repeat<T>(defaultStr, delim2));
    used.insert(name);
    return array;
  }
  void warn_unused(ostream &out, bool removeUnused = true) {
    Vector<string> unused;
    LOOP(const PSS & p, params) {
      if (!in(used, p.first)) {
        unused += p.first;
      }
    }
    if (unused.size()) {
      LOOP(string & s, unused) {
        out << "WARNING: " << s << " in config but never used" << endl;
        if (removeUnused) {
          params.erase(s);
        }
      }
      out << endl;
    }
  }
};

static ostream &operator<<(ostream &out, const ConfigFile &conf) {
  out << conf.params;
  return out;
}

#endif
