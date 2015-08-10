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

#ifndef _INCLUDED_String_h
#define _INCLUDED_String_h

#include "Helpers.hpp"
#include "Container.hpp"

static string ordinal(size_t n) {
  string s = str(n);
  if (n < 100) {
    char c = nth_last(s);
    if (c == '1') {
      return s + "st";
    } else if (c == '2') {
      return s + "nd";
    } else if (c == '3') {
      return s + "rd";
    }
  }
  return s + "th";
}
static void trim(string &str) {
  size_t startpos = str.find_first_not_of(" \t\n");
  size_t endpos = str.find_last_not_of(" \t\n");
  if (string::npos == startpos || string::npos == endpos) {
    str = "";
  } else {
    str = str.substr(startpos, endpos - startpos + 1);
  }
}
static const string lower(const string &s) {
  static string l;
  l = s;
  algorithm::to_lower(l);
  return l;
}
static bool in(const string &str, const string &search) {
  return (str.find(search) != string::npos);
}
static bool in(const string &str, const char *search) {
  return in(str, string(search));
}
static bool begins(const string &str, const string &search) {
  return (str.find(search) == 0);
}
static bool begins(const string &str, const char *search) {
  return begins(str, string(search));
}
static bool ends(const string &str, const string &search) {
  return (str.find(search, str.size() - search.size()) != string::npos);
}
static bool ends(const string &str, const char *search) {
  return ends(str, string(search));
}
template <class T>
static Vector<T> split(const string &original, char delim = ' ',
                       size_t maxSplits = 0) {
  Vector<T> vect;
  stringstream ss;
  ss << original;
  string s;
  while (delim == ' ' ? ss >> s : getline(ss, s, delim)) {
    vect += read<T>(s);
    if (vect.size() == maxSplits - 1) {
      delim = '\0';
    }
  }
  return vect;
}
template <class T>
static Vector<T> split_with_repeat(const string &original, char delim = ' ',
                                   char repeater = '*') {
  Vector<T> vect;
  LOOP(const string & s1, split<string>(original, delim)) {
    vector<string> v = split<string>(s1, repeater);
    size_t numRepeats = (v.size() == 1 ? 1 : natural(v[1]));
    T val = read<T>(v[0]);
    vect += val, repeat(numRepeats - 1, val);
  }
  return vect;
}
template <class T, class R>
static string join(const R &r, const string joinStr = "") {
  typename range_iterator<R>::type b = boost::begin(r);
  string s = str(*b);
  ++b;
  for (; b != end(r); ++b) {
    s += joinStr + str(*b);
  }
  return s;
}

template <class T> string left_pad(const T &val, int width, char fill = '0') {
  ostringstream ss;
  ss << setw(width) << setfill(fill) << val;
  return ss.str();
}

static string int_to_sortable_string(size_t num, size_t max) {
  assert(num < max);
  return left_pad(num, str(max - 1).size());
}

#endif
