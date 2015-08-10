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

#ifndef _INCLUDED_LstmLayer_h
#define _INCLUDED_LstmLayer_h

#include "Layer.hpp"
#include "Matrix.hpp"
#include "WeightContainer.hpp"
#define PEEPS

template <class CI, class CO, class G> struct LstmLayer : public Layer {
  // data
  size_t numBlocks;
  size_t cellsPerBlock;
  size_t numCells;
  size_t gatesPerBlock;
  size_t unitsPerBlock;
  size_t peepsPerBlock;
  SeqBuffer<real_t> inGateActs;
  SeqBuffer<real_t> forgetGateActs;
  SeqBuffer<real_t> outGateActs;
  SeqBuffer<real_t> preOutGateActs;
  SeqBuffer<real_t> states;
  SeqBuffer<real_t> preGateStates;
  SeqBuffer<real_t> cellErrors;
  vector<vector<int> > stateDelays;
  vector<int> delayedCoords;
  vector<View<real_t> > oldStates;
  vector<View<real_t> > nextErrors;
  vector<View<real_t> > nextFgActs;
  vector<View<real_t> > nextCellErrors;
#ifdef PEEPS
  LstmLayer<CI, CO, G> *peepSource;
  pair<size_t, size_t> peepRange;
#endif

  // functions
  LstmLayer(const string &name, const vector<int> &directions, size_t nb,
            size_t cpb = 1, LstmLayer<CI, CO, G> *ps = 0)
      : Layer(name, directions, (cpb + directions.size() + 2) * nb, nb),
        numBlocks(nb), cellsPerBlock(cpb), numCells(numBlocks * cellsPerBlock),
        gatesPerBlock(this->num_seq_dims() + 2),
        unitsPerBlock(gatesPerBlock + cellsPerBlock),
        peepsPerBlock(gatesPerBlock * cellsPerBlock), inGateActs(numBlocks),
        forgetGateActs(numBlocks * this->num_seq_dims()),
        outGateActs(numBlocks), preOutGateActs(numCells), states(numCells),
        preGateStates(numCells), cellErrors(numCells),
        stateDelays(this->num_seq_dims()), delayedCoords(this->num_seq_dims()),
        oldStates(this->num_seq_dims()), nextErrors(this->num_seq_dims()),
        nextFgActs(this->num_seq_dims()), nextCellErrors(this->num_seq_dims())
#ifdef PEEPS
        ,
        peepSource(ps),
        peepRange(peepSource ? peepSource->peepRange
                             : WeightContainer::instance().new_parameters(
                                   peepsPerBlock * numBlocks, name, name,
                                   name + "_peepholes"))
#endif
  {
    if (peepSource) {
      WeightContainer::instance().link_layers(
          name, name, name + "_peepholes", peepRange.first, peepRange.second);
    }

    // initialise the state delays
    LOOP(int i, span(this->num_seq_dims())) {
      stateDelays[i].resize(this->num_seq_dims(), 0);
      stateDelays[i][i] = -directions[i];
    }
    // export the data
    display(this->inputActivations, "inputActivations");
    display(this->outputActivations, "outputActivations");
    display(this->inputErrors, "inputErrors");
    display(this->outputErrors, "outputErrors");
    DISPLAY(cellErrors);
    DISPLAY(states);
    DISPLAY(inGateActs);
    DISPLAY(forgetGateActs);
    DISPLAY(outGateActs);
  }
  ~LstmLayer() {}
  void start_sequence() {
    Layer::start_sequence();
    inGateActs.reshape(this->output_seq_shape());
    forgetGateActs.reshape(this->output_seq_shape());
    outGateActs.reshape(this->output_seq_shape());
    preOutGateActs.reshape(this->output_seq_shape());
    states.reshape(this->output_seq_shape());
    preGateStates.reshape(this->output_seq_shape());
    cellErrors.reshape(states);
  }
  void feed_forward(const vector<int> &coords) {
    real_t *actBegin = this->outputActivations[coords].begin();
    real_t *inActIt = this->inputActivations[coords].begin();
    real_t *inGateActBegin = inGateActs[coords].begin();
    real_t *fgActBegin = forgetGateActs[coords].begin();
    real_t *outGateActBegin = outGateActs[coords].begin();
    real_t *stateBegin = states[coords].begin();
    real_t *preGateStateBegin = preGateStates[coords].begin();
    real_t *preOutGateActBegin = preOutGateActs[coords].begin();
    LOOP(int d, span(this->num_seq_dims())) {
      oldStates[d] =
          states.at(range_plus(delayedCoords, coords, stateDelays[d]));
    }
#ifdef PEEPS
    const real_t *peepWtIt =
        WeightContainer::instance().get_weights(peepRange).begin();
#endif
    int cellStart = 0;
    int cellEnd = cellsPerBlock;
    real_t *fgActEnd = fgActBegin + this->num_seq_dims();
    LOOP(int b, span(numBlocks)) {
#ifdef PEEPS
      View<real_t> fgActs(fgActBegin, fgActEnd);
      // input gate
      // extra inputs from peepholes (from old states)
      LOOP(const View<real_t> & os, oldStates) {
        if (os.begin()) {
          dot(os.begin() + cellStart, os.begin() + cellEnd, peepWtIt, inActIt,
              inActIt + 1);
        }
      }
      peepWtIt += cellsPerBlock;
#endif
      real_t inGateAct = G::fn(*inActIt);
      inGateActBegin[b] = inGateAct;
      ++inActIt;

      // forget gates
      // extra inputs from peepholes (from old states)
      LOOP(int d, span(this->num_seq_dims())) {
#ifdef PEEPS
        const View<real_t> &os = oldStates[d];
        if (os.begin()) {
          dot(os.begin() + cellStart, os.begin() + cellEnd, peepWtIt, inActIt,
              inActIt + 1);
        }
        peepWtIt += cellsPerBlock;
#endif
        fgActs[d] = G::fn(*inActIt);
        ++inActIt;
      }

      // pre-gate cell states
      transform(inActIt, inActIt + cellsPerBlock, preGateStateBegin + cellStart,
                CI::fn);
      inActIt += cellsPerBlock;

      // cell states
      LOOP(int c, span(cellStart, cellEnd)) {
        real_t state = inGateAct * preGateStateBegin[c];
        LOOP(int d, span(this->num_seq_dims())) {
          const View<real_t> &os = oldStates[d];
          if (os.begin()) {
            state += fgActs[d] * os[c];
          }
        }
        stateBegin[c] = state;
        preOutGateActBegin[c] = CO::fn(state);
      }

// output gate
// extra input from peephole (from current state)
#ifdef PEEPS
      dot(stateBegin + cellStart, stateBegin + cellEnd, peepWtIt, inActIt,
          inActIt + 1);
      peepWtIt += cellsPerBlock;
#endif

      real_t outGateAct = G::fn(*inActIt);
      outGateActBegin[b] = outGateAct;
      ++inActIt;

      // output activations
      transform(preOutGateActBegin + cellStart, preOutGateActBegin + cellEnd,
                actBegin + cellStart,
                bind2nd(multiplies<real_t>(), outGateAct));
      cellStart = cellEnd;
      cellEnd += cellsPerBlock;
      fgActBegin = fgActEnd;
      fgActEnd += this->num_seq_dims();
    }
  }
  void feed_back(const vector<int> &coords) {
    // activations
    const real_t *inGateActBegin = inGateActs[coords].begin();
    const real_t *forgetGateActBegin = forgetGateActs[coords].begin();
    const real_t *outGateActBegin = outGateActs[coords].begin();
    const real_t *preGateStateBegin = preGateStates[coords].begin();
    const real_t *preOutGateActBegin = preOutGateActs[coords].begin();

    // errors
    View<real_t> inErrs = this->inputErrors[coords];
    real_t *cellErrorBegin = cellErrors[coords].begin();
    const real_t *outputErrorBegin = this->outputErrors[coords].begin();
    real_t *errorIt = inErrs.begin();
#ifdef PEEPS
    const real_t *peepWtIt =
        WeightContainer::instance().get_weights(peepRange).begin();
#endif
    LOOP(int d, span(this->num_seq_dims())) {
      oldStates[d] =
          states.at(range_plus(delayedCoords, coords, stateDelays[d]));
      range_minus(delayedCoords, coords, stateDelays[d]);
      nextErrors[d] = this->inputErrors.at(delayedCoords);
      nextFgActs[d] = forgetGateActs.at(delayedCoords);
      nextCellErrors[d] = cellErrors.at(delayedCoords);
    }
    int cellStart = 0;
    int cellEnd = cellsPerBlock;
    int fgStart = 0;
    int gateStart = 0;
    LOOP(int b, span(numBlocks)) {
      real_t inGateAct = inGateActBegin[b];
      real_t outGateAct = outGateActBegin[b];

      // output gate error
      real_t outGateError = G::deriv(outGateAct) *
                            inner_product(preOutGateActBegin + cellStart,
                                          preOutGateActBegin + cellEnd,
                                          outputErrorBegin + cellStart, 0.0);

      // cell pds (dE/dState)
      LOOP(int c, span(cellStart, cellEnd)) {
        real_t deriv = (CO::deriv(preOutGateActBegin[c]) * outGateAct *
                        outputErrorBegin[c]);
#ifdef PEEPS
        int cOffset = c - cellStart;
        real_t igPeepWt = peepWtIt[cOffset];
        real_t ogPeepWt = peepWtIt[peepsPerBlock - cellsPerBlock + cOffset];
        deriv += outGateError * ogPeepWt;
#endif
        LOOP(int d, span(this->num_seq_dims())) {
#ifdef PEEPS
          real_t fgPeepWt = peepWtIt[cOffset + (cellsPerBlock * (d + 1))];
#endif
          const View<real_t> &nextErrs = nextErrors[d];
          if (nextErrs.begin()) {
#ifdef PEEPS
            deriv += (nextErrs[gateStart + 1 + d] * fgPeepWt) +
                     (nextErrs[gateStart] * igPeepWt);
#endif
            deriv += (nextFgActs[d][fgStart + d] * nextCellErrors[d][c]);
          }
        }
        cellErrorBegin[c] = deriv;
      }

      // input gate error
      *errorIt =
          G::deriv(inGateAct) *
          inner_product(cellErrorBegin + cellStart, cellErrorBegin + cellEnd,
                        preGateStateBegin + cellStart, 0.0);
      ++errorIt;

      // forget gate error
      LOOP(int d, span(this->num_seq_dims())) {
        const View<real_t> &os = oldStates[d];
        if (os.begin()) {
          *errorIt = G::deriv(forgetGateActBegin[fgStart + d]) *
                     inner_product(cellErrorBegin + cellStart,
                                   cellErrorBegin + cellEnd,
                                   os.begin() + cellStart, 0.0);
        } else {
          *errorIt = 0;
        }
        ++errorIt;
      }

      // cell errors
      LOOP(int c, span(cellStart, cellEnd)) {
        *errorIt =
            inGateAct * CI::deriv(preGateStateBegin[c]) * cellErrorBegin[c];
        ++errorIt;
      }
      *errorIt = outGateError;
      ++errorIt;
#ifdef PEEPS
      peepWtIt += peepsPerBlock;
#endif
      cellStart += cellsPerBlock;
      cellEnd += cellsPerBlock;
      fgStart += this->num_seq_dims();
      gateStart += unitsPerBlock;
    }

    // constrain errors to be in [-10,10] for stability
    if (!runningGradTest) {
      bound_range(inErrs, -10.0, 10.0);
    }
  }
#ifdef PEEPS
  void update_derivs(const vector<int> &coords) {
    const real_t *stateBegin = states[coords].begin();
    const real_t *errorBegin = this->inputErrors[coords].begin();
    real_t *pdIt = WeightContainer::instance().get_derivs(peepRange).begin();
    LOOP(int d, span(this->num_seq_dims())) {
      oldStates[d] =
          states.at(range_plus(delayedCoords, coords, stateDelays[d]));
    }
    LOOP(int b, span(numBlocks)) {
      int cellStart = b * cellsPerBlock;
      int cellEnd = cellStart + cellsPerBlock;
      int errorOffset = b * unitsPerBlock;
      real_t inGateError = errorBegin[errorOffset];
      LOOP(int d, span(this->num_seq_dims())) {
        const View<real_t> &os = oldStates[d];
        if (os.begin()) {
          LOOP(int c, span(cellStart, cellEnd)) {
            pdIt[c - cellStart] += inGateError * os[c];
          }
          real_t forgGateError = errorBegin[errorOffset + d + 1];
          LOOP(int c, span(cellStart, cellEnd)) {
            pdIt[(c - cellStart) + ((d + 1) * cellsPerBlock)] +=
                forgGateError * os[c];
          }
        }
      }
      real_t outGateError = errorBegin[errorOffset + unitsPerBlock - 1];
      LOOP(int c, span(cellStart, cellEnd)) {
        pdIt[(c - cellStart) + peepsPerBlock - cellsPerBlock] +=
            outGateError * stateBegin[c];
      }
      pdIt += peepsPerBlock;
    }
  }
  void print(ostream &out = cout) const {
    Layer::print(out);
    out << " " << difference(peepRange) << " peeps";
    if (peepSource) {
      out << " (shared with " << peepSource->name << ")";
    }
  }
  const View<real_t> weights() {
    return WeightContainer::instance().get_weights(peepRange);
  }
#endif
};

#endif
