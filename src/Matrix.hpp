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

#ifndef _INCLUDED_Matrix_h
#define _INCLUDED_Matrix_h
extern "C" {
#include "cblas.h"
}

#define OP_TRACKING

#ifdef OP_TRACKING
static unsigned long long matrixOps = 0;
#endif

// M += a * b
static void outer(const double *aBegin, const double *aEnd, double *M,
                  const double *b, const double *bEnd) {
  cblas_dger(CblasRowMajor, bEnd - b, aEnd - aBegin, 1.0, b, 1, aBegin, 1, M,
             aEnd - aBegin);
#ifdef OP_TRACKING
  matrixOps += (aEnd - aBegin) * (bEnd - b);
#endif
}
static void outer(const float *aBegin, const float *aEnd, float *M,
                  const float *b, const float *bEnd) {
  cblas_sger(CblasRowMajor, bEnd - b, aEnd - aBegin, 1.0, b, 1, aBegin, 1, M,
             aEnd - aBegin);
#ifdef OP_TRACKING
  matrixOps += (aEnd - aBegin) * (bEnd - b);
#endif
}

// out += M in
static void dot(const real_t *inBegin, const real_t *inEnd, const real_t *M,
                real_t *out, real_t *outEnd) {
#ifdef FLOAT_REALS
  cblas_sgemv
#else
  cblas_dgemv
#endif
      (CblasRowMajor, CblasNoTrans, outEnd - out, inEnd - inBegin, 1.0, M,
       inEnd - inBegin, inBegin, 1, 1, out, 1);
#ifdef OP_TRACKING
  matrixOps += (outEnd - out) * (inEnd - inBegin);
#endif
}

static void dot(const real_t *inBegin, const real_t *inEnd, size_t inDepth,
                const real_t *W, real_t *out, real_t *outEnd, size_t outDepth) {
  size_t M = (inEnd - inBegin) / inDepth;
  size_t N = outDepth;
  size_t K = inDepth;
#ifdef FLOAT_REALS
  cblas_sgemm
#else
  cblas_dgemm
#endif
      (CblasRowMajor, CblasNoTrans, CblasTrans, M, N, K, 1.0, inBegin, inDepth,
       W, inDepth, 1.0, out, outDepth);
#ifdef OP_TRACKING
  matrixOps += (outEnd - out) * (inEnd - inBegin);
#endif
}

// out += transpose(M) in
static void dot_transpose(const real_t *inBegin, const real_t *inEnd,
                          const real_t *M, real_t *out, real_t *outEnd) {
#ifdef FLOAT_REALS
  cblas_sgemv
#else
  cblas_dgemv
#endif
      (CblasRowMajor, CblasTrans, inEnd - inBegin, outEnd - out, 1.0, M,
       outEnd - out, inBegin, 1, 1, out, 1);
#ifdef OP_TRACKING
  matrixOps += (outEnd - out) * (inEnd - inBegin);
#endif
}

// out += transpose(M^2) in
static void dot_transpose_m_squared(const real_t *in, const real_t *inEnd,
                                    const real_t *M, real_t *outBegin,
                                    real_t *outEnd) {
#ifdef OP_TRACKING
  const real_t *mStart = M;
#endif
  for (; in != inEnd; ++in) {
    real_t input = *in;
    for (real_t *out = outBegin; out != outEnd; ++out, ++M) {
      *out += squared(*M) * input;
    }
  }
#ifdef OP_TRACKING
  matrixOps += M - mStart;
#endif
}
// out = x.*y
static void ele_mul(const real_t *x, const real_t *xEnd, const real_t *y,
                    const real_t beta, real_t *out) {
  assert(out != y && out != x);
  int n = xEnd - x;
#ifdef FLOAT_REALS
  cblas_ssbmv
#else
  cblas_dsbmv
#endif
      (CblasRowMajor, CblasUpper, n, 0, 1.0, x, 1, y, 1, beta, out, 1);
}
// M += a^2 * b
static void outer_a_squared(const real_t *aBegin, const real_t *aEnd, real_t *M,
                            const real_t *b, const real_t *bEnd) {
#ifdef OP_TRACKING
  const real_t *mStart = M;
#endif
  for (; b != bEnd; ++b) {
    real_t input = *b;
    for (const real_t *a = aBegin; a != aEnd; ++a, ++M) {
      *M += squared(*a) * input;
    }
  }
#ifdef OP_TRACKING
  matrixOps += M - mStart;
#endif
}
template <class R> static void outer(const R &a, real_t *M, const R &b) {
  outer(boost::begin(a), boost::end(a), M, boost::begin(b), boost::end(b));
}
template <class R> static void dot(const R &a, const real_t *M, const R &b) {
  dot(boost::begin(a), boost::end(a), M, boost::begin(b), boost::end(b));
}
template <class R>
static void dot_transpose(const R &a, const real_t *M, const R &b) {
  dot_transpose(boost::begin(a), boost::end(a), M, boost::begin(b),
                boost::end(b));
}
template <class R>
static void outer_a_squared(const R &a, real_t *M, const R &b) {
  outer_a_squared(boost::begin(a), boost::end(a), M, boost::begin(b),
                  boost::end(b));
}
template <class R>
static void dot_transpose_m_squared(const R &a, const real_t *M, const R &b) {
  dot_transpose_m_squared(boost::begin(a), boost::end(a), M, boost::begin(b),
                          boost::end(b));
}
#endif
