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

#ifndef _INCLUDED_Container_h
#define _INCLUDED_Container_h

template <class T> struct Vector;

template <class T> struct View : public sub_range<pair<T *, T *> > {
  View(pair<T *, T *> &p) : sub_range<pair<T *, T *> >(p) {}
  View(T *first = 0, T *second = 0)
      : sub_range<pair<T *, T *> >(make_pair(first, second)) {}
  View slice(int first = 0, int last = numeric_limits<int>::max()) {
    first = bound(first, 0, (int)this->size());
    if (last < 0) {
      last += (int)this->size();
    }
    last = bound(last, first, (int)this->size());
    int diff = last - first;
    return View(&((*this)[first]), &((*this)[first]) + diff);
  }
  View slice(pair<int, int> &r) { return slice(r.first, r.second); }
  const View slice(int first = 0, int last = numeric_limits<int>::max()) const {
    return const_cast<View *>(this)->slice(first, last);
  }
  const View slice(pair<int, int> &r) const { return slice(r.first, r.second); }
  T &at(size_t i) {
    check(i < this->size(),
          "at(" + str(i) + ") called for view of size " + str(this->size()));
    return (*this)[i];
  }
  const T &at(size_t i) const {
    check(i < this->size(),
          "at(" + str(i) + ") called for view of size " + str(this->size()));
    return (*this)[i];
  }
  template <class R> const View<T> &operator=(const R &r) const {
    check(boost::size(r) == this->size(),
          "can't assign range " + str(r) + " to view " + str(*this));
    copy(r, *this);
    return *this;
  }
  template <class T2> Vector<T2> to() const {
    Vector<T2> v;
    LOOP(const T & t, *this) { v += lexical_cast<T2>(t); }
    return v;
  }
};

template <class T> struct Vector : public vector<T> {
  Vector() {}
  Vector(const vector<T> &v) : vector<T>(v) {}
  Vector(const View<const T> &v) { *this = v; }
  Vector(size_t n) : vector<T>(n) {}
  Vector(size_t n, const T &t) : vector<T>(n, t) {}
  Vector<T> &grow(size_t length) {
    this->resize(this->size() + length);
    return *this;
  }
  Vector<T> &shrink(size_t length) {
    this->resize(max((size_t)0, this->size() - length));
    return *this;
  }
  void push_front(const T &t) { this->insert(this->begin(), t); }
  View<T> slice(int first = 0, int last = numeric_limits<int>::max()) {
    first = bound(first, 0, (int)this->size());
    if (last < 0) {
      last += (int)this->size();
    }
    last = bound(last, first, (int)this->size());
    return View<T>(&((*this)[first]), &((*this)[last]));
  }
  View<T> slice(pair<int, int> &r) { return slice(r.first, r.second); }
  const View<T> slice(int first = 0,
                      int last = numeric_limits<int>::max()) const {
    return slice(first, last);
  }
  const View<T> slice(pair<int, int> &r) const {
    return slice(r.first, r.second);
  }
  template <class R> Vector<T> &extend(const R &r) {
    size_t oldSize = this->size();
    grow(boost::size(r));
    copy(boost::begin(r), boost::end(r), this->begin() + oldSize);
    return *this;
  }
  Vector<T> replicate(size_t times) const {
    Vector<T> v;
    REPEAT(times) { v.extend(*this); }
    return v;
  }
  template <class R> Vector<T> &operator=(const R &r) {
    this->resize(boost::size(r));
    copy(r, *this);
    return *this;
  }
  template <class T2> Vector<T2> to() const {
    Vector<T2> v;
    LOOP(const T & t, *this) { v += lexical_cast<T2>(t); }
    return v;
  }
};

template <class T> struct Set : public set<T> {
  Set() {}
  Set(const vector<T> &v) { *this = v; }
  Set(const View<T> &v) { *this = v; }
  Set &operator+=(const T &val) {
    this->insert(val);
    return *this;
  }
  template <class R> Set<T> &operator=(const R &r) {
    this->clear();
    return this->extend(r);
  }
  template <class R> Set<T> &extend(const R &r) {
    LOOP(const typename boost::range_value<R>::type & val, r) {
      (*this) += val;
    }
    return *this;
  }
};

#endif
