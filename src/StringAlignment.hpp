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

#ifndef _INCLUDED_StringAlignment_h
#define _INCLUDED_StringAlignment_h

#include <vector>
#include <map>
#include <iostream>
#include "Helpers.hpp"

;

template <class R1, class R2> struct StringAlignment {
  // data
  map<typename boost::range_value<R1>::type,
      map<typename boost::range_value<R1>::type, int> > subsMap;
  map<typename boost::range_value<R1>::type, int> delsMap;
  map<typename boost::range_value<R1>::type, int> insMap;
  Vector<Vector<int> > matrix;
  int substitutions;
  int deletions;
  int insertions;
  int distance;
  int subPenalty;
  int delPenalty;
  int insPenalty;
  size_t n;
  size_t m;

  // functions
  StringAlignment(const R1 &reference_sequence, const R2 &test_sequence,
                  bool trackErrors = false, bool backtrace = true, int sp = 1,
                  int dp = 1, int ip = 1)
      : subPenalty(sp), delPenalty(dp), insPenalty(ip),
        n(reference_sequence.size()), m(test_sequence.size()) {
    if (n == 0) {
      substitutions = 0;
      deletions = 0;
      insertions = m;
      distance = m;
    } else if (m == 0) {
      substitutions = 0;
      deletions = n;
      insertions = 0;
      distance = n;
    } else {
      // initialise the matrix
      matrix.resize(n + 1);
      LOOP(Vector<int> & v, matrix) {
        v.resize(m + 1);
        fill(v, 0);
      }
      LOOP(int i, span(n + 1)) { matrix[i][0] = i; }
      LOOP(int j, span(m + 1)) { matrix[0][j] = j; }

      // calculate the insertions, substitutions and deletions
      LOOP(int i, span(1, n + 1)) {
        const typename boost::range_value<R1>::type &s_i =
            reference_sequence[i - 1];
        LOOP(int j, span(1, m + 1)) {
          const typename boost::range_value<R2>::type &t_j =
              test_sequence[j - 1];
          int cost = ((s_i == t_j) ? 0 : 1);
          const int above = matrix[i - 1][j];
          const int left = matrix[i][j - 1];
          const int diag = matrix[i - 1][j - 1];
          const int cell = min(above + 1,         // deletion
                               min(left + 1,      // insertion
                                   diag + cost)); // substitution

          matrix[i][j] = cell;
        }
      }

      // N.B sub,ins and del penalties are all set to 1 if backtrace is ignored
      if (backtrace) {
        size_t i = n;
        size_t j = m;
        substitutions = 0;
        deletions = 0;
        insertions = 0;

        // Backtracking
        while (i != 0 && j != 0) {
          if (matrix[i][j] == matrix[i - 1][j - 1]) {
            --i;
            --j;
          } else if (matrix[i][j] == matrix[i - 1][j - 1] + 1) {
            if (trackErrors) {
              ++subsMap[reference_sequence[i]][test_sequence[j]];
            }
            ++substitutions;
            --i;
            --j;
          } else if (matrix[i][j] == matrix[i - 1][j] + 1) {
            if (trackErrors) {
              ++delsMap[reference_sequence[i]];
            }
            ++deletions;
            --i;
          } else {
            if (trackErrors) {
              ++insMap[test_sequence[j]];
            }
            ++insertions;
            --j;
          }
        }
        while (i != 0) {
          if (trackErrors) {
            ++delsMap[reference_sequence[i]];
          }
          ++deletions;
          --i;
        }
        while (j != 0) {
          if (trackErrors) {
            ++insMap[test_sequence[j]];
          }
          ++insertions;
          --j;
        }

        // Sanity check:
        check((substitutions + deletions + insertions) == matrix[n][m],
              "Found path with distance " +
                  str(substitutions + deletions + insertions) +
                  " but Levenshtein distance is " + str(matrix[n][m]));

        // scale individual errors by penalties
        distance = (subPenalty * substitutions) + (delPenalty * deletions) +
                   (insPenalty * insertions);
      } else {
        distance = matrix[n][m];
      }
    }
  }
  ~StringAlignment() {}
};

#endif
