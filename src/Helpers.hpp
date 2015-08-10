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

#ifndef _INCLUDED_Helpers_h
#define _INCLUDED_Helpers_h

#include <boost/date_time.hpp>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/array.hpp>
#include <boost/timer.hpp>
#include <boost/assign/std/vector.hpp>
#include <boost/range.hpp>
#include <boost/iterator/counting_iterator.hpp>
#include <boost/iterator/zip_iterator.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/minmax_element.hpp>
#include <boost/bimap.hpp>
#include <boost/foreach.hpp>
#include <boost/math/distributions.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/range/irange.hpp>
#include <boost/tuple/tuple.hpp>
#include <math.h>
#include <numeric>
#include <utility>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <sstream>
#include <list>
#include <set>
#include <algorithm>
#include <iterator>
#include <map>
#include <assert.h>
#include <cblas.h>
#include "Log.hpp"
#include "fromstd.hpp"

using namespace boost;
using namespace boost::assign;
using namespace boost::posix_time;
using namespace boost::gregorian;

#define LOOP BOOST_FOREACH
#define LOOP_BACK BOOST_REVERSE_FOREACH
#define DO(x, y) BOOST_FOREACH(BOOST_TYPEOF(*((y).begin())) & (x), (y))
#define DOC(x, y) BOOST_FOREACH(const BOOST_TYPEOF(*((y).begin())) & (x), (y))
#define OD(x, y) BOOST_REVERSE_FOREACH(BOOST_TYPEOF(*((y).begin())) & (x), (y))
#define COD(x, y)                                                              \
  BOOST_REVERSE_FOREACH(const BOOST_TYPEOF(*((y).begin())) & (x), (y))
#define REPEAT(n) for (int REPEAT_VARn = 0; REPEAT_VARn < (n); ++REPEAT_VARn)
#define FROM(i, m, n) for (int(i) = (m); (i) < (n); ++(i))
#define MORF(i, m, n) for (int(i) = (n) - 1; (i) >= (m); --(i))
#define FOR(i, n) for (int(i) = 0; (i) < (n); ++(i))
#define ROF(i, n) for (int(i) = (n) - 1; (i) >= 0; --(i))

//#define FLOAT_REALS

#ifdef FLOAT_REALS
typedef float real_t;
#else
typedef double real_t;
#endif

typedef vector<size_t>::const_iterator VSTCI;
typedef vector<real_t>::iterator VDI;
typedef vector<real_t>::const_iterator VDCI;
typedef vector<real_t>::reverse_iterator VDRI;
typedef string::iterator SI;
typedef string::const_iterator SCI;
typedef vector<int>::iterator VII;
typedef vector<string>::iterator VSI;
typedef vector<string>::const_iterator VSCI;
typedef vector<int>::reverse_iterator VIRI;
typedef vector<vector<int> >::reverse_iterator VVIRI;
typedef vector<int>::const_iterator VICI;
typedef vector<bool>::iterator VBI;
typedef vector<real_t>::iterator VFI;
typedef vector<real_t>::const_iterator VFCI;
typedef vector<vector<real_t> >::iterator VVDI;
typedef vector<vector<real_t> >::const_iterator VVDCI;
typedef vector<vector<int> >::iterator VVII;
typedef vector<vector<int> >::const_iterator VVICI;
typedef vector<unsigned int>::iterator VUII;
typedef vector<vector<real_t> >::iterator VVFI;
typedef map<string, string>::iterator MSSI;
typedef map<string, string>::const_iterator MSSCI;
typedef map<string, real_t>::iterator MSDI;
typedef map<string, real_t>::const_iterator MSDCI;
typedef map<string, pair<int, real_t> >::iterator MSPIDI;
typedef map<string, pair<int, real_t> >::const_iterator MSPIDCI;
typedef vector<map<string, pair<int, real_t> > >::const_iterator VMSDCI;
typedef vector<map<string, pair<int, real_t> > >::iterator VMSDI;
typedef vector<map<string, pair<int, real_t> > >::reverse_iterator VMSDRI;
typedef map<string, int>::iterator MSII;
typedef map<string, int>::const_iterator MSICI;
typedef map<int, int>::iterator MIII;
typedef map<int, int>::const_iterator MIICI;
typedef vector<vector<int> >::const_reverse_iterator VVIRCI;
typedef vector<int>::const_reverse_iterator VIRCI;
typedef vector<const real_t *>::const_iterator VPCFCI;
typedef vector<const real_t *>::iterator VPCFI;
typedef vector<const real_t *>::const_reverse_iterator VPCFCRI;
typedef vector<bool>::const_iterator VBCI;
typedef vector<bool>::iterator VBI;
typedef map<string, pair<real_t, int> >::iterator MCSPDII;
typedef map<string, pair<real_t, int> >::const_iterator MCSPDICI;
typedef bimap<int, string>::left_const_iterator BMISLCI;
typedef bimap<int, string>::right_const_iterator BMISRCI;
typedef bimap<int, string>::relation BMISR;
typedef pair<string, real_t> PSD;
typedef pair<int, int> PII;
typedef pair<const string, real_t> PCSD;
typedef pair<string, int> PSI;
typedef pair<int, string> PIS;
typedef pair<string, string> PSS;
typedef const boost::tuples::tuple<real_t &, real_t &, real_t &, real_t &> &
TDDDD;
typedef const tuple<real_t &, real_t &, real_t &, real_t &, real_t &> &TDDDDD;
typedef const tuple<real_t &, real_t &, real_t &> &TDDD;
typedef const tuple<real_t &, real_t &, int &> &TDDI;
typedef const tuple<real_t &, real_t &, real_t &> &TDDF;
typedef const tuple<real_t &, real_t &, real_t> &TDDCF;
typedef const tuple<real_t &, real_t &> &TDD;
typedef const tuple<int, string> &TIS;
typedef const tuple<int, int> &TII;
typedef const tuple<int, real_t> &TID;
typedef const tuple<int, set<int> &> &TISETI;
typedef const tuple<int &, bool, int> &TIBI;
typedef const tuple<real_t, Log<real_t> &> &TDL;
typedef const tuple<real_t &, Log<real_t>, Log<real_t> > &TDLL;
typedef Log<real_t> prob_t;
typedef pair<real_t, real_t> PDD;

// global variables
#define realMax numeric_limits<real_t>::max()
#define realMin numeric_limits<real_t>::min()
#define realInfinity numeric_limits<real_t>::infinity()
static const real_t almostZero = 1e-4;

extern bool runningGradTest;
extern bool verbose;
extern ostream &COUT;
extern void dumpState();
#define PRINT(x, o) ((o) << boolalpha << #x " = " << (x) << endl)
#define PRINTN(x, o)                                                           \
  (o) << boolalpha << #x ":" << endl;                                          \
  print_range((o), (x), string("\n"));                                         \
  (o) << endl
#define PRT(x) PRINT(x, cout)
#define PRTN(x) PRINTN(x, cout)
#define PRINTR(x, o, d)                                                        \
  (o) << boolalpha << #x " = ";                                                \
  print_range((o), (x), str(d));                                               \
  (o) << endl
#define PRTR(x, d) PRINTR((x), cout, (d))
#define check(condition, str)                                                  \
  if (!(condition)) {                                                          \
    cout << endl << "ERRROR: " << (str) << endl << endl;                       \
    (assert((condition)));                                                     \
  }
#define CHECK_STRICT(condition, str)                                           \
  if (!(condition)) {                                                          \
    cout << endl << "ERRROR: " << (str) << endl << endl;                       \
    dumpState();                                                               \
    (assert((condition)));                                                     \
    exit(0);                                                                   \
  }

// MISC FUNCTIONS
static bool warn_unless(bool condition, const string &str,
                        ostream &out = cout) {
  if (!condition) {
    out << "WARNING: " << str << endl;
  }
  return condition;
}
static void print_time(real_t totalSeconds, ostream &out = cout,
                       bool abbrv = false) {
  int wholeSeconds = floor(totalSeconds);
  int seconds = wholeSeconds % 60;
  int totalMinutes = wholeSeconds / 60;
  int minutes = totalMinutes % 60;
  int totalHours = totalMinutes / 60;
  int hours = totalHours % 24;
  int totalDays = totalHours / 24;
  int days = totalDays % 365;
  if (days) {
    out << days << " day";
    if (days > 1) {
      out << "s";
    }
    out << " ";
  }
  if (hours) {
    out << hours << (abbrv ? " hr" : " hour");
    if (hours > 1) {
      out << "s";
    }
    out << " ";
  }
  if (minutes) {
    out << minutes << (abbrv ? " min" : " minute");
    if (minutes > 1) {
      out << "s";
    }
    out << " ";
  }
  out << totalSeconds - wholeSeconds + seconds
      << (abbrv ? " secs" : " seconds");
}
static string time_stamp(const string &format = "%Y.%m.%d-%H.%M.%S%F%Q") {
  time_facet *timef = new time_facet(format.c_str());
  stringstream ss;
  ss.imbue(locale(ss.getloc(), timef));
  ss << microsec_clock::local_time();
  return ss.str();
}
static void mark() {
  static int num = 0;
  cout << "MARK " << num << endl;
  ++num;
}
template <class T> static T squared(const T &t) { return t * t; }
template <class T> static int sign(const T &t) {
  if (t < 0) {
    return -1;
  } else if (t > 0) {
    return 1;
  } else {
    return 0;
  }
}
template <class T>
static T bound(const T &v, const T &minVal, const T &maxVal) {
  return min(max(minVal, v), maxVal);
}
// CAST OPERATIONS

template <class T> static string str(const T &t);
template <class T> static real_t real(const T &t) {
  return lexical_cast<real_t>(t);
}
template <class T> static int integer(const T &t) {
  return lexical_cast<int>(t);
}
template <class T> static size_t natural(const T &t) {
  return lexical_cast<size_t>(t);
}
// GENERIC RANGE OPERATIONS
template <class R>
static void flood(R &r, size_t size,
                  const typename boost::range_value<R>::type &v = 0) {
  r.resize(size);
  fill(r, v);
}
template <class R, class T>
static typename range_iterator<R>::type find(R &r, const T &t) {
  return find(boost::begin(r), boost::end(r), t);
}
template <class R1, class R2>
static vector<typename boost::range_value<R1>::type> &
select_channels(const R1 &r, const R2 &channels) {
  static vector<typename boost::range_value<R1>::type> v;
  v.clear();
  LOOP(int i, channels) {
    check(in_range(r, i), "channel " + str(i) + " in\n  " + str(channels) +
                              "\nout of bounds for range\n" + str(r));
    v += r[i];
  }
  return v;
}
template <class R> static size_t count_adjacent(const R &r) {
  size_t count = 0;
  for (typename range_iterator<R>::type it = boost::begin(r);
       it != boost::end(r); it = adjacent_find(it, boost::end(r))) {
    ++count;
  }
  return count;
}
template <class R1, class R2>
static pair<zip_iterator<tuple<typename range_iterator<R1>::type,
                               typename range_iterator<R2>::type> >,
            zip_iterator<tuple<typename range_iterator<R1>::type,
                               typename range_iterator<R2>::type> > >
zip(R1 &r1, R2 &r2) {
  size_t size = range_min_size(r1, r2);
  return make_pair(make_zip_iterator(boost::tuples::make_tuple(
                       boost::begin(r1), boost::begin(r2))),
                   make_zip_iterator(boost::tuples::make_tuple(
                       boost::end(r1) - (boost::size(r1) - size),
                       boost::end(r2) - (boost::size(r2) - size))));
}

template <class R1, class R2>
static size_t range_min_size(const R1 &a, const R2 &b);
template <class R1, class R2, class R3>
static size_t range_min_size(const R1 &a, const R2 &b, const R3 &c);
template <class R1, class R2, class R3, class R4>
static size_t range_min_size(const R1 &a, const R2 &b, const R3 &c,
                             const R4 &d);
template <class R1, class R2, class R3, class R4, class R5>
static size_t range_min_size(const R1 &a, const R2 &b, const R3 &c, const R4 &d,
                             const R5 &e);

template <class R1, class R2, class R3>
static pair<zip_iterator<tuple<typename range_iterator<R1>::type,
                               typename range_iterator<R2>::type,
                               typename range_iterator<R3>::type> >,
            zip_iterator<tuple<typename range_iterator<R1>::type,
                               typename range_iterator<R2>::type,
                               typename range_iterator<R3>::type> > >
zip(R1 &r1, R2 &r2, R3 &r3) {
  size_t size = range_min_size(r1, r2, r3);
  return make_pair(make_zip_iterator(boost::tuples::make_tuple(
                       boost::begin(r1), boost::begin(r2), boost::begin(r3))),
                   make_zip_iterator(boost::tuples::make_tuple(
                       boost::end(r1) - (boost::size(r1) - size),
                       boost::end(r2) - (boost::size(r2) - size),
                       boost::end(r3) - (boost::size(r3) - size))));
}
template <class R1, class R2, class R3, class R4>
static pair<
    zip_iterator<tuple<
        typename range_iterator<R1>::type, typename range_iterator<R2>::type,
        typename range_iterator<R3>::type, typename range_iterator<R4>::type> >,
    zip_iterator<tuple<typename range_iterator<R1>::type,
                       typename range_iterator<R2>::type,
                       typename range_iterator<R3>::type,
                       typename range_iterator<R4>::type> > >
zip(R1 &r1, R2 &r2, R3 &r3, R4 &r4) {
  size_t size = range_min_size(r1, r2, r3, r4);
  return make_pair(make_zip_iterator(boost::tuples::make_tuple(
                       boost::begin(r1), boost::begin(r2), boost::begin(r3),
                       boost::begin(r4))),
                   make_zip_iterator(boost::tuples::make_tuple(
                       boost::end(r1) - (boost::size(r1) - size),
                       boost::end(r2) - (boost::size(r2) - size),
                       boost::end(r3) - (boost::size(r3) - size),
                       boost::end(r4) - (boost::size(r4) - size))));
}
template <class R1, class R2, class R3, class R4, class R5>
static pair<
    zip_iterator<tuple<
        typename range_iterator<R1>::type, typename range_iterator<R2>::type,
        typename range_iterator<R3>::type, typename range_iterator<R4>::type,
        typename range_iterator<R5>::type> >,
    zip_iterator<tuple<
        typename range_iterator<R1>::type, typename range_iterator<R2>::type,
        typename range_iterator<R3>::type, typename range_iterator<R4>::type,
        typename range_iterator<R5>::type> > >
zip(R1 &r1, R2 &r2, R3 &r3, R4 &r4, R5 &r5) {
  size_t size = range_min_size(r1, r2, r3, r4, r5);
  return make_pair(make_zip_iterator(boost::tuples::make_tuple(
                       boost::begin(r1), boost::begin(r2), boost::begin(r3),
                       boost::begin(r4), boost::begin(r5))),
                   make_zip_iterator(boost::tuples::make_tuple(
                       boost::end(r1) - (boost::size(r1) - size),
                       boost::end(r2) - (boost::size(r2) - size),
                       boost::end(r3) - (boost::size(r3) - size),
                       boost::end(r4) - (boost::size(r4) - size),
                       boost::end(r5) - (boost::size(r5) - size))));
}
// template <class T1, class T2> static pair<counting_iterator<T2>,
// counting_iterator<T2> > span(const T1& t1, const T2& t2)
template <class T1, class T2> static integer_range<T2> span(T1 t1, T2 t2) {
  return irange<T2>((t1 < t2 ? static_cast<T2>(t1) : t2), t2);
  // return make_pair(counting_iterator<T2>(t1 < t2 ? static_cast<T2>(t1) : t2),
  // counting_iterator<T2>(t2));
}
// template <class T> static pair<counting_iterator<T>, counting_iterator<T> >
// span(const T& t)
template <class T> static integer_range<T> span(T t) {
  return span(0, t);
  //	return make_pair(counting_iterator<T>(0), counting_iterator<T>(t));
}
// template <class R> static pair<counting_iterator<typename
// range_difference<R>::type>, counting_iterator<typename
// range_difference<R>::type> > indices(const R& r)
template <class R>
static integer_range<typename boost::range_size<R>::type> indices(const R &r) {
  return span(boost::size(r));
}
template <class R>
static pair<
    zip_iterator<tuple<counting_iterator<typename range_difference<R>::type>,
                       typename range_iterator<R>::type> >,
    zip_iterator<tuple<counting_iterator<typename range_difference<R>::type>,
                       typename range_iterator<R>::type> > >
enumerate(R &r) {
  return make_pair(
      make_zip_iterator(boost::tuples::make_tuple(
          counting_iterator<typename range_difference<R>::type>(0),
          boost::begin(r))),
      make_zip_iterator(boost::tuples::make_tuple(
          counting_iterator<typename range_difference<R>::type>(boost::size(r)),
          boost::end(r))));
}
template <class T1, class T2> static vector<T1> iota(const T2 &t) {
  vector<T1> v;
  LOOP(const T2 & val, span(t)) { v += lexical_cast<T1>(val); }
  return v;
}
template <class T1, class T2>
static vector<T1> iota(const T2 &t1, const T2 &t2) {
  vector<T1> v;
  LOOP(const T2 & val, span(t1, t2)) { v += lexical_cast<T1>(val); }
  return v;
}
template <class R1, class R2, class F>
static typename range_iterator<R2>::type transform(const R1 &r1, R2 &r2,
                                                   const F &f) {
  return transform(boost::begin(r1), boost::end(r1), boost::begin(r2), f);
}
template <class R> static bool in_range(R &r, size_t n) {
  return n >= 0 && n < boost::size(r);
}
template <class R>
static typename range_value<R>::type &nth_last(R &r, size_t n = 1) {
  check(in_range(r, n - 1), "nth_last called with n = " + str(n) +
                                " for range of size " + (str(boost::size(r))));
  return *(boost::end(r) - n);
}
template <class R> static size_t last_index(R &r) {
  return (boost::size(r) - 1);
}
template <class R, class UnaryFunction>
static UnaryFunction for_each(R &r, UnaryFunction f) {
  return for_each(boost::begin(r), boost::end(r), f);
}
template <class R, class T> static bool in(const R &r, const T &t) {
  return find(r, t) != boost::end(r);
}
template <class R, class T> static size_t index(const R &r, const T &t) {
  return distance(boost::begin(r), find(r, t));
}
template <class R> static void reverse(R &r) {
  reverse(boost::begin(r), boost::end(r));
}
template <class R> static R &sort(R &r) {
  sort(boost::begin(r), boost::end(r));
  return r;
}
template <class R> static R &reverse_sort(R &r) {
  reverse(sort(r));
  return r;
}
template <class R>
pair<typename range_value<R>::type, typename range_value<R>::type>
minmax(const R &r) {
  pair<typename range_const_iterator<R>::type,
       typename range_const_iterator<R>::type> p =
      boost::minmax_element(boost::begin(r), boost::end(r));
  return make_pair(*p.first, *p.second);
}
template <class R>
static void bound_range(R &r,
                        const typename boost::range_value<R>::type &minVal,
                        const typename boost::range_value<R>::type &maxVal) {
  for (typename range_iterator<R>::type it = boost::begin(r);
       it != boost::end(r); ++it) {
    *it = bound(*it, minVal, maxVal);
  }
}
template <class R1, class R2>
typename boost::range_value<R1>::type euclidean_squared(const R1 &r1,
                                                        const R2 &r2) {
  typename range_const_iterator<R2>::type b2 = boost::begin(r2);
  typename range_const_iterator<R1>::type e = boost::end(r1);
  typename boost::range_value<R1>::type d = 0;
  for (typename range_const_iterator<R1>::type b1 = boost::begin(r1); b1 != e;
       ++b1, ++b2) {
    typename boost::range_value<R1>::type diff = *b1 - *b2;
    d += diff * diff;
  }
  return d;
}
template <class R>
static void fill(R &r, const typename boost::range_value<R>::type &v) {
  fill(boost::begin(r), boost::end(r), v);
}
template <class R>
static size_t count(const R &r, const typename boost::range_value<R>::type &v) {
  return count(boost::begin(r), boost::end(r), v);
}
template <class R1, class R2> static void copy(const R1 &source, R2 &dest) {
  assert(boost::size(dest) >= boost::size(source));
  copy(boost::begin(source), boost::end(source), boost::begin(dest));
}
template <class R1, class R2>
static void reverse_copy(const R1 &source, R2 &dest) {
  reverse_copy(boost::begin(source), boost::end(source), boost::begin(dest));
}
template <class R, class T> void vector_assign(const R &r, vector<T> &v) {
  v.resize(boost::size(r));
  copy(r, v);
}
template <class R> static void range_negate_equals(R &r) {
  transform(boost::begin(r), boost::end(r), boost::begin(r),
            negate<typename boost::range_value<R>::type>());
}
template <class R>
static vector<typename boost::range_value<R>::type> &range_negate(const R &r) {
  static vector<typename boost::range_value<R>::type> v;
  vector_assign(r, v);
  range_negate_equals(v);
  return v;
}
template <class R>
static vector<typename boost::range_value<R>::type> &flip(const R &r) {
  static vector<typename boost::range_value<R>::type> v;
  v.resize(boost::size(r));
  reverse_copy(r, v);
  return v;
}
template <class R1, class R2> static bool equal(const R1 &source, R2 &dest) {
  return ((boost::size(source) == boost::size(dest)) &&
          equal(boost::begin(source), boost::end(source), boost::begin(dest)));
}
template <class R> static R &shuffle(R &r) {
  random_shuffle(boost::begin(r), boost::end(r));
  return r;
}
template <class R> static typename range_value<R>::type max(const R &r) {
  return *max_element(boost::begin(r), boost::end(r));
}

template <class C, class Tr, class R>
static void print_range(basic_ostream<C, Tr> &out, const R &r,
                        const basic_string<C, Tr> &delim = " ") {
  typename range_const_iterator<R>::type b = boost::begin(r);
  typename range_const_iterator<R>::type e = boost::end(r);
  //    typename range_iterator<const R>::type b = boost::begin(r);
  //    typename range_iterator<const R>::type e = boost::end(r);
  if (b != e) {
    out << (*b);
    while (++b != e) {
      out << delim << (*b);
    }
  }
}
template <class C, class Tr, class R>
static void print_range(basic_ostream<C, Tr> &out, const R &r,
                        const char delim) {
  print_range(out, r, str(delim));
}
template <class C, class Tr, class R>
static basic_ostream<C, Tr> &operator<<(basic_ostream<C, Tr> &out, const R &r) {
  print_range(out, r);
  return out;
}
template <class C, class Tr, class R>
static basic_istream<C, Tr> &operator>>(basic_istream<C, Tr> &in, R &r) {
  typename range_iterator<R>::type b = boost::begin(r);
  typename range_iterator<R>::type e = boost::end(r);
  for (; b != e; ++b) {
    in >> *b;
  }
  return in;
}
template <class R> void delete_range(R &r) {
  for (typename range_iterator<R>::type it = boost::begin(r);
       it != boost::end(r); ++it) {
    delete *it;
  }
}
template <class R1, class R2>
static size_t range_min_size(const R1 &a, const R2 &b) {
  return min(boost::size(a), boost::size(b));
}
template <class R1, class R2, class R3>
static size_t range_min_size(const R1 &a, const R2 &b, const R3 &c) {
  return min(min(boost::size(a), boost::size(b)), boost::size(c));
}
template <class R1, class R2, class R3, class R4>
static size_t range_min_size(const R1 &a, const R2 &b, const R3 &c,
                             const R4 &d) {
  return min(min(min(boost::size(a), boost::size(b)), boost::size(c)),
             boost::size(d));
}
template <class R1, class R2, class R3, class R4, class R5>
static size_t range_min_size(const R1 &a, const R2 &b, const R3 &c, const R4 &d,
                             const R5 &e) {
  return min(min(min(min(boost::size(a), boost::size(b)), boost::size(c)),
                 boost::size(d)),
             boost::size(e));
}
template <class R> static int arg_max(const R &r) {
  return distance(boost::begin(r), max_element(boost::begin(r), boost::end(r)));
}
// ARITHMETIC RANGE OPERATIONS
template <class T1, class T2, class T3, class T4>
static T1 gauss_pdf(const T2 &x, const T3 &mean, const T4 &stdDev) {
  math::normal_distribution<T3> d(mean, stdDev);
  return T1(math::pdf(d, x));
}
template <class T, class R1, class R2, class R3>
static T range_indep_gauss_pdf(const R1 &variables, const R2 &means,
                               const R3 &stdDevs) {
  assert(boost::size(variables) == boost::size(means));
  assert(boost::size(means) == boost::size(stdDevs));
  T prob(1);
  typename range_iterator<R1>::type v = boost::begin(variables);
  typename range_iterator<R1>::type end = boost::end(variables);
  typename range_iterator<R2>::type mean = boost::begin(means);
  typename range_iterator<R3>::type sd = boost::begin(stdDevs);
  for (; v != end; ++v, ++mean, ++sd) {
    prob *= gauss_pdf(*v, *mean, *sd);
  }
  return prob;
}
template <typename C>
C inner_product(const real_t *a, const real_t *aEnd, const real_t *b, C c) {
  if (c == 0) {
    return
#ifdef FLOAT_REALS
        cblas_sdot
#else
        cblas_ddot
#endif
        (aEnd - a, a, 1, b, 1);
  }
  return std::inner_product(a, aEnd, b, c);
}
template <typename C>
C inner_product(real_t *a, real_t *aEnd, const real_t *b, C c) {
  return inner_product(const_cast<const real_t *>(a),
                       const_cast<const real_t *>(aEnd),
                       const_cast<const real_t *>(b), c);
}
template <typename C> C inner_product(real_t *a, real_t *aEnd, real_t *b, C c) {
  return inner_product(const_cast<const real_t *>(a),
                       const_cast<const real_t *>(aEnd),
                       const_cast<const real_t *>(b), c);
}

template <class R1, class R2>
static typename range_value<R1>::type
inner_product(const R1 &a, const R2 &b, typename range_value<R1>::type c = 0) {
  return inner_product(boost::begin(a), boost::end(a), boost::begin(b), c);
}
template <class R> static typename range_value<R>::type norm(const R &r) {
  return sqrt(inner_product(r, r));
}
template <class R1, class R2>
static typename range_value<R1>::type sum_of_squares(const R1 &r1,
                                                     const R2 &r2) {
  typename range_const_iterator<R1>::type it1 = boost::begin(r1);
  typename range_const_iterator<R2>::type it2 = boost::begin(r2);
  typename range_const_iterator<R1>::type e = boost::end(r1);
  typename range_value<R1>::type v = 0;
  for (; it1 != e; ++it1, ++it2) {
    typename range_value<R1>::type diff = *it1 - *it2;
    v += diff * diff;
  }
  return v / 2;
}
template <class R> static typename range_value<R>::type product(const R &r) {
  return accumulate(boost::begin(r), boost::end(r),
                    (typename range_value<R>::type)1,
                    multiplies<typename range_value<R>::type>());
}
template <class R> static typename range_value<R>::type sum(const R &r) {
  return accumulate(boost::begin(r), boost::end(r),
                    (typename range_value<R>::type)0);
}
template <class R> static typename range_value<R>::type abs_sum(const R &r) {
  typename range_const_iterator<R>::type e = boost::end(r);
  typename range_value<R>::type v = 0;
  for (typename range_const_iterator<R>::type it = boost::begin(r); it != e;
       ++it) {
    v += abs(*it);
  }
  return v;
}
template <class R> static typename range_value<R>::type log_sum(const R &r) {
  typename range_const_iterator<R>::type e = boost::end(r);
  typename range_value<R>::type v = 0;
  for (typename range_const_iterator<R>::type it = boost::begin(r); it != e;
       ++it) {
    v += Log<typename range_value<R>::type>::safe_log(*it);
  }
  return v;
}
template <class R> static typename range_value<R>::type mean(const R &r) {
  return sum(r) / (typename range_value<R>::type)boost::size(r);
}
template <class R> static typename range_value<R>::type variance(const R &r) {
  typename range_value<R>::type M = mean(r);
  typename range_value<R>::type v = 0;
  typename range_const_iterator<R>::type e = boost::end(r);
  for (typename range_const_iterator<R>::type it = boost::begin(r); it != e;
       ++it) {
    v += squared(*it - M);
  }
  return v / boost::size(r);
}
template <class R> static typename range_value<R>::type std_dev(const R &r) {
  return sqrt(variance(r));
}
// plus
template <class R1, class R2>
static void range_plus_val(R1 &a, const R2 &b,
                           const typename boost::range_value<R2>::type &c) {
  transform(boost::begin(b), boost::end(b), boost::begin(a),
            bind2nd(plus<typename boost::range_value<R2>::type>(), c));
}
template <class R>
static void range_plus_val(R &a,
                           const typename boost::range_value<R>::type &b) {
  range_plus_val(a, a, b);
}
template <class R1, class R2, class R3>
static R1 &range_plus(R1 &a, const R2 &b, const R3 &c) {
  transform(boost::begin(b), boost::end(b), boost::begin(c), boost::begin(a),
            plus<typename boost::range_value<R1>::type>());
  return a;
}
template <class R1, class R2>
static void range_plus_equals(R1 &a, const R2 &b) {
  range_plus(a, a, b);
}
// minus
template <class R1, class R2>
static void range_minus_val(R1 &a, const R2 &b,
                            const typename boost::range_value<R2>::type &c) {
  transform(boost::begin(b), boost::end(b), boost::begin(a),
            bind2nd(minus<typename boost::range_value<R2>::type>(), c));
}
template <class R>
static void range_minus_val(R &a,
                            const typename boost::range_value<R>::type &b) {
  range_minus_val(a, a, b);
}
template <class R1, class R2, class R3>
static void range_minus(R1 &a, const R2 &b, const R3 &c) {
  transform(boost::begin(b), boost::end(b), boost::begin(c), boost::begin(a),
            minus<typename boost::range_value<R1>::type>());
}
template <class R1, class R2>
static void range_minus_equals(R1 &a, const R2 &b) {
  range_minus(a, a, b);
}
// multiply
template <class R1, class R2>
static void range_multiply_val(R1 &a, const R2 &b,
                               const typename boost::range_value<R2>::type &c) {
  transform(boost::begin(b), boost::end(b), boost::begin(a),
            bind2nd(multiplies<typename boost::range_value<R2>::type>(), c));
}
template <class R>
static void range_multiply_val(R &a,
                               const typename boost::range_value<R>::type &b) {
  range_multiply_val(a, a, b);
}
template <class R1, class R2, class R3>
static void range_multiply(R1 &a, const R2 &b, const R3 &c) {
  transform(boost::begin(b), boost::begin(b) + range_min_size(a, b, c),
            boost::begin(c), boost::begin(a),
            multiplies<typename boost::range_value<R1>::type>());
}
template <class R1, class R2>
static void range_multiply_equals(R1 &a, const R2 &b) {
  range_multiply(a, a, b);
}
template <class R1, class R2, class R3>
static void range_multiply_add(R1 &a, const R2 &b, const R3 &c) {
  typename range_iterator<R1>::type ab = boost::begin(a);
  typename range_iterator<R1>::type ae = boost::end(a);
  typename range_const_iterator<R2>::type bb = boost::begin(b);
  typename range_const_iterator<R3>::type cb = boost::begin(c);
  for (; ab != ae; ++ab, ++bb, ++cb) {
    *ab += *bb * *cb;
  }
}
// divide a = b./c
template <class R1, class R2>
static void range_divide_val(R1 &a, const R2 &b,
                             const typename boost::range_value<R1>::type &c) {
  transform(boost::begin(b), boost::end(b), boost::begin(a),
            bind2nd(divides<typename boost::range_value<R1>::type>(), c));
}
template <class R>
static void range_divide_val(R &a,
                             const typename boost::range_value<R>::type &b) {
  range_divide_val(a, a, b);
}
template <class R1, class R2, class R3>
static void range_divide(R1 &a, const R2 &b, const R3 &c) {
  transform(boost::begin(b), boost::end(b), boost::begin(c), boost::begin(a),
            divides<typename boost::range_value<R1>::type>());
}
template <class R1, class R2>
static void range_divide_equals(R1 &a, const R2 &b) {
  range_divide(a, a, b);
}
// TUPLE OPERATIONS
template <class T1, class T2>
static ostream &operator<<(ostream &out, const tuple<T1, T2> &t) {
  //	out << (t.get<0>()) << " " << t.get<1>();
  out << (get<0>(t)) << " " << get<1>(t);
  return out;
}
template <class T1, class T2, class T3>
static ostream &operator<<(ostream &out, const tuple<T1, T2, T3> &t) {
  //	out << t.get<0>() << " " << t.get<1>() << " " << t.get<2>();
  out << get<0>(t) << " " << get<1>(t) << " " << get<2>(t);
  return out;
}
template <class T1, class T2, class T3, class T4>
static ostream &operator<<(ostream &out, const tuple<T1, T2, T3, T4> &t) {
  //	out << t.get<0>() << " " << t.get<1>() << " " << t.get<2>() << " " <<
  // t.get<3>();
  out << get<0>(t) << " " << get<1>(t) << " " << get<2>(t) << " " << get<3>(t);
  return out;
}
template <class T1, class T2, class T3, class T4, class T5>
static ostream &operator<<(ostream &out, const tuple<T1, T2, T3, T4, T5> &t) {
  //	out << t.get<0>() << " " << t.get<1>() << " " << t.get<2>() << " " <<
  // t.get<3>() << " " << t.get<4>();
  out << get<0>(t) << " " << get<1>(t) << " " << get<2>(t) << " " << get<3>(t)
      << " " << get<4>(t);
  return out;
}
// PAIR OPERATIONS
template <class T> static bool in_open_interval(pair<T, T> interval, T val) {
  return ((val > interval.first) && (val < interval.second));
}
template <class T> static bool in_closed_interval(pair<T, T> interval, T val) {
  return ((val >= interval.first) && (val <= interval.second));
}
template <class T1, class T2>
static void operator+=(pair<T1, T2> &a, const pair<T1, T2> &b) {
  a.first += b.first;
  a.second += b.second;
}
template <class T1, class T2, class T3>
static pair<T1, T2> operator+(const pair<T1, T2> &a, const T3 &b) {
  return make_pair(a.first + b, a.second + b);
}
template <class T1, class T2>
static ostream &operator<<(ostream &out, const pair<T1, T2> &p) {
  out << p.first << " " << p.second;
  return out;
}
template <class T1, class T2>
static real_t pair_product(const pair<T1, T2> &p) {
  return (real_t)(p.first * p.second);
}
template <class T1, class T2> static real_t pair_sum(const pair<T1, T2> &p) {
  return (real_t)(p.first + p.second);
}
template <class T1, class T2> static real_t pair_mean(const pair<T1, T2> &p) {
  return pair_sum(p) / 2.0;
}
template <class T1, class T2> static size_t difference(const pair<T1, T2> &p) {
  return p.second - p.first;
}
// MAP OPERATIONS
template <class T1, class T2>
static bool in(const map<T1, T2> &a, const T1 &b) {
  return (a.find(b) != a.end());
}
template <class T1, class T2>
static const T2 &at(const map<T1, T2> &a, const T1 &b) {
  typename map<T1, T2>::const_iterator it = a.find(b);
  check(it != a.end(), str(b) + " not found in map:\n" + str(a));
  return it->second;
}
template <class T1, class T2>
static void print_left(const map<T1, T2> &m, ostream &out = cout,
                       const char delim = ' ') {
  for (typename map<T1, T2>::const_iterator it = m.begin(); it != m.end();
       ++it) {
    out << it->first << delim;
  }
}
template <class T1, class T2>
static void print_right(const map<T1, T2> &m, ostream &out = cout,
                        const char delim = ' ') {
  for (typename map<T1, T2>::const_iterator it = m.begin(); it != m.end();
       ++it) {
    out << it->second << delim;
  }
}
template <class T1, class T2>
static ostream &operator<<(ostream &out, const map<T1, T2> &m) {
  for (typename map<T1, T2>::const_iterator it = m.begin(); it != m.end();
       ++it) {
    out << *it << endl;
  }
  return out;
}
template <class T1, class T2>
static ostream &operator<<(ostream &out, const map<T1, T2 *> &m) {
  for (typename map<T1, T2 *>::const_iterator it = m.begin(); it != m.end();
       ++it) {
    out << it->first << " " << *(it->second) << endl;
  }
  return out;
}
template <class T1, class T2> static T2 sum_right(const map<T1, T2> &m) {
  T2 ret = 0;
  for (typename map<T1, T2>::const_iterator it = m.begin(); it != m.end();
       ++it) {
    ret += it->second;
  }
  return ret;
}
template <class T1, class T2, class T3, class T4>
static void operator+=(map<T1, T2> &a, const map<T3, T4> &b) {
  for (typename map<T3, T4>::const_iterator it = b.begin(); it != b.end();
       ++it) {
    a[it->first] += it->second;
  }
}
template <class T1, class T2, class T3, class T4>
static void operator-=(map<T1, T2> &a, const map<T3, T4> &b) {
  for (typename map<T3, T4>::const_iterator it = b.begin(); it != b.end();
       ++it) {
    a[it->first] -= it->second;
  }
}
template <class T1, class T2, class T3, class T4>
static void operator/=(map<T1, T2> &a, const map<T3, T4> &b) {
  for (typename map<T3, T4>::const_iterator it = b.begin(); it != b.end();
       ++it) {
    a[it->first] /= it->second;
  }
}
template <class T1, class T2, class T3, class T4>
static void operator*=(map<T1, T2> &a, const map<T3, T4> &b) {
  for (typename map<T3, T4>::const_iterator it = b.begin(); it != b.end();
       ++it) {
    a[it->first] *= it->second;
  }
}
template <class T1, class T2, class T3>
static void operator*=(map<T1, T2> &a, const T3 &b) {
  for (typename map<T1, T2>::iterator it = a.begin(); it != a.end(); ++it) {
    it->second *= b;
  }
}
template <class T1, class T2, class T3>
static void operator/=(map<T1, T2> &a, const T3 &b) {
  for (typename map<T1, T2>::iterator it = a.begin(); it != a.end(); ++it) {
    it->second /= b;
  }
}
template <class R> void delete_map(R &r) {
  for (typename range_iterator<R>::type it = boost::begin(r);
       it != boost::end(r); ++it) {
    delete it->second;
  }
}
// MULTIMAP OPERATIONS
template <class T1, class T2>
static bool in(const multimap<T1, T2> &a, const T1 &b) {
  return (a.find(b) != a.end());
}
// BIMAP OPERATIONS
template <class T1, class T2, class T3, class T4>
static ostream &operator<<(
    ostream &out,
    const boost::bimaps::relation::structured_pair<T1, T2, T3, T4> &p) {
  out << p.first << " " << p.second;
  return out;
}
template <class T1, class T2>
void print_bimap(const bimap<T1, T2> &m, ostream &out) {
  for (typename bimap<T1, T2>::left_const_iterator it = m.left.begin();
       it != m.left.end(); ++it) {
    out << *it << endl;
  }
}
template <class T1, class T2>
static ostream &operator<<(ostream &out, const bimap<T1, T2> &m) {
  print_bimap(m, out);
  return out;
}
template <class T1, class T2>
static bool in_left(const bimap<T1, T2> &a, const T1 &b) {
  return (a.left.find(b) != a.left.end());
}
template <class T1, class T2>
static bool in_right(const bimap<T1, T2> &a, const T2 &b) {
  return (a.right.find(b) != a.right.end());
}
// IO OPERATIONS
template <class T> static void print(const T &t, ostream &out = cout) {
  out << t << endl;
}
template <class T1, class T2>
static void print(const T1 &t1, const T2 &t2, ostream &out = cout) {
  out << t1 << " " << t2 << endl;
}
template <class T1, class T2, class T3>
static void print(const T1 &t1, const T2 &t2, const T3 &t3,
                  ostream &out = cout) {
  out << t1 << " " << t2 << " " << t3 << endl;
}
template <class T1, class T2, class T3, class T4>
static void print(const T1 &t1, const T2 &t2, const T3 &t3, const T4 &t4,
                  ostream &out = cout) {
  out << t1 << " " << t2 << " " << t3 << " " << t4 << endl;
}
template <class T1, class T2, class T3, class T4, class T5>
static void print(const T1 &t1, const T2 &t2, const T3 &t3, const T4 &t4,
                  const T5 &t5, ostream &out = cout) {
  out << t1 << " " << t2 << " " << t3 << " " << t4 << " " << t5 << endl;
}
static void prt_line(ostream &out = cout) {
  out << "------------------------------" << endl;
}
template <class T> static T read(const string &data) {
  T val;
  stringstream ss;
  ss << boolalpha << data;
  check(ss >> val, "cannot read string '" + data +
                       "' into variable with type '" + typeid(T).name() + "'");
  return val;
}
// SET OPERATIONS
template <class T> static bool disjoint(const set<T> &s1, const set<T> &s2) {
  static set<T> result;
  result.clear();
  set_intersection(s1.begin(), s1.end(), s2.begin(), s2.end(),
                   insert_iterator<set<T> >(result, result.begin()));
  return result.empty();
}
template <class T>
static bool intersecting(const set<T> &s1, const set<T> &s2) {
  return !disjoint(s1, s2);
}
template <class T> static assign_detail::generic_list<T> empty_list_of() {
  return assign_detail::generic_list<T>();
}
// PROBABILITY FUNCTIONS
static real_t KL_normal(real_t pMean, real_t pVar, real_t qMean, real_t qVar) {
  qVar = std::max(realMin, qVar);
  pVar = std::max(realMin, pVar);
  real_t qpVar = std::max(realMin, qVar / pVar);
  real_t sqMean =
      std::min(squared(pMean - qMean), numeric_limits<real_t>::max());
  real_t term =
      std::min(((sqMean + pVar) / qVar), numeric_limits<real_t>::max());
  real_t rv = 0.5 * (log(qpVar) - 1 + term);

  // org    real_t rv =  0.5 * (log(qVar/pVar) - 1 + ((squared(pMean - qMean) +
  // pVar) / qVar));
  if (!boost::math::isfinite(rv)) {
    COUT << "qpVar:" << qpVar << " term :" << term << endl;
    CHECK_STRICT(boost::math::isfinite(rv), "KL is off");
  }
  return rv;
}
static real_t nats_to_bits(real_t nats) {
  static real_t F = 1.0 / Log<real_t>::safe_log(2);
  return F * nats;
}
static real_t bits_to_nats(real_t bits) {
  static real_t F = Log<real_t>::safe_log(2);
  return F * bits;
}
template <class T> static string str(const T &t) {
  stringstream ss;
  ss << t;
  return ss.str();
  //	return lexical_cast<string>(t);
}
#endif
