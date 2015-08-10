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

#ifndef rnnlib_Lstm1dLayer_hpp
#define rnnlib_Lstm1dLayer_hpp

#include "LstmLayer.hpp"

template <class CI, class CO, class G>
struct Lstm1dLayer : public LstmLayer<CI, CO, G> {
  using LstmLayer<CI, CO, G>::inputActivations;
  using LstmLayer<CI, CO, G>::outputActivations;
  using LstmLayer<CI, CO, G>::numBlocks;
  using LstmLayer<CI, CO, G>::cellsPerBlock;
  using LstmLayer<CI, CO, G>::numCells;
  using LstmLayer<CI, CO, G>::gatesPerBlock;
  using LstmLayer<CI, CO, G>::unitsPerBlock;
  using LstmLayer<CI, CO, G>::peepsPerBlock;
  using LstmLayer<CI, CO, G>::inGateActs;
  using LstmLayer<CI, CO, G>::forgetGateActs;
  using LstmLayer<CI, CO, G>::outGateActs;
  using LstmLayer<CI, CO, G>::preOutGateActs;
  using LstmLayer<CI, CO, G>::states;
  using LstmLayer<CI, CO, G>::preGateStates;
  using LstmLayer<CI, CO, G>::cellErrors;
  using LstmLayer<CI, CO, G>::stateDelays;
  using LstmLayer<CI, CO, G>::delayedCoords;
  using LstmLayer<CI, CO, G>::oldStates;
  using LstmLayer<CI, CO, G>::nextErrors;
  using LstmLayer<CI, CO, G>::nextFgActs;
  using LstmLayer<CI, CO, G>::nextCellErrors;
  using LstmLayer<CI, CO, G>::num_seq_dims;
  using LstmLayer<CI, CO, G>::peepRange;

  Vector<real_t> dervOutGateAct;
  Vector<real_t> dervTempCell;
  Vector<real_t> dropoutMask;
  real_t dropoutProb;
  enum DROPOUT_STATE {
    DROPOUT_STATE_NONE,
    DROPOUT_STATE_TRAINING,
    DROPOUT_STATE_VALIDATING
  } dropoutState;

  Lstm1dLayer(const string &name, const vector<int> &directions, size_t nb,
              size_t cpb = 1, LstmLayer<CI, CO, G> *ps = 0)
      : LstmLayer<CI, CO, G>(name, directions, nb, cpb, ps), dervOutGateAct(nb),
        dervTempCell(nb), dropoutMask(nb), dropoutProb(.5),
        dropoutState(DROPOUT_STATE_NONE) {}
  void start_sequence() {
    LstmLayer<CI, CO, G>::start_sequence();
    if (dropoutState == DROPOUT_STATE_TRAINING) {
      for (auto m_i = dropoutMask.begin(); m_i != dropoutMask.end(); m_i++) {
        *m_i = Random::bernoulli(dropoutProb);
      }
    } else if (dropoutState == DROPOUT_STATE_VALIDATING) {
      fill(dropoutMask, dropoutProb);
    }
  }
  void set_dropout_mode(bool is_training) {

    if (is_training) {
      COUT << this->name
           << " setting dropout state to 'training' p=" << dropoutProb << endl;
      dropoutState = DROPOUT_STATE_TRAINING;
    } else {
      COUT << this->name
           << " setting dropout state to 'validating' p=" << dropoutProb
           << endl;
      dropoutState = DROPOUT_STATE_VALIDATING;
    }
  }
  void feed_forward(const vector<int> &coords) {
    real_t *actBegin = this->outputActivations[coords].begin();
    real_t *cellGateActIt = this->inputActivations[coords].begin();
    real_t *outGateIt = cellGateActIt + numBlocks;
    real_t *forgetGateIt = cellGateActIt + 2 * numBlocks;
    real_t *inputGateIt = cellGateActIt + 3 * numBlocks;

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
    const real_t *peepInputWtIt =
        WeightContainer::instance().get_weights(peepRange).begin();
    const real_t *peepOutputWtIt = peepInputWtIt + numBlocks;
    const real_t *peepForgetWtIt = peepInputWtIt + 2 * numBlocks;
#endif
    // input gate
    // extra inputs from peepholes (from old states)
    LOOP(const View<real_t> & os, oldStates) {
      if (os.begin()) {
        // input gate
        // extra inputs from peepholes (from old states)
        ele_mul(os.begin(), os.begin() + numBlocks, peepInputWtIt, 1,
                inputGateIt);
        // forget gates
        // extra inputs from peepholes (from old states)
        ele_mul(os.begin(), os.begin() + numBlocks, peepForgetWtIt, 1,
                forgetGateIt);
      }
    }
    transform(inputGateIt, inputGateIt + numBlocks, inGateActBegin, G::fn);
    transform(forgetGateIt, forgetGateIt + numBlocks, fgActBegin, G::fn);
    // cell state
    transform(cellGateActIt, cellGateActIt + numBlocks, preGateStateBegin,
              CI::fn);
    ele_mul(preGateStateBegin, preGateStateBegin + numBlocks, inGateActBegin, 0,
            stateBegin);
    LOOP(const View<real_t> & os, oldStates) {
      if (os.begin()) {
        ele_mul(fgActBegin, fgActBegin + numBlocks, os.begin(), 1.0,
                stateBegin);
      }
    }
    transform(stateBegin, stateBegin + numBlocks, preOutGateActBegin, CO::fn);

    // output gate
    // extra input from peephole (from current state)
    ele_mul(stateBegin, stateBegin + numBlocks, peepOutputWtIt, 1, outGateIt);
    transform(outGateIt, outGateIt + numBlocks, outGateActBegin, G::fn);
    ele_mul(preOutGateActBegin, preOutGateActBegin + numBlocks, outGateActBegin,
            0, actBegin);
    if (dropoutState != DROPOUT_STATE_NONE) {
      ele_mul(actBegin, actBegin + numBlocks, dropoutMask.slice().begin(), 0,
              dervTempCell.slice().begin());
      copy(dervTempCell, this->outputActivations[coords]);
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
    real_t *errorCellGateActIt = inErrs.begin();
    real_t *errorOutGateIt = errorCellGateActIt + numBlocks;
    real_t *errorForgetGateIt = errorCellGateActIt + 2 * numBlocks;
    real_t *errorInputGateIt = errorCellGateActIt + 3 * numBlocks;

#ifdef PEEPS
    const real_t *peepInputWtIt =
        WeightContainer::instance().get_weights(peepRange).begin();
    const real_t *peepOutputWtIt = peepInputWtIt + numBlocks;
    const real_t *peepForgetWtIt = peepInputWtIt + 2 * numBlocks;
#endif
    LOOP(int d, span(this->num_seq_dims())) {
      oldStates[d] =
          states.at(range_plus(delayedCoords, coords, stateDelays[d]));
      range_minus(delayedCoords, coords, stateDelays[d]);
      nextErrors[d] = this->inputErrors.at(delayedCoords);
      nextFgActs[d] = forgetGateActs.at(delayedCoords);
      nextCellErrors[d] = cellErrors.at(delayedCoords);
    }
    // output err
    transform(outGateActBegin, outGateActBegin + numBlocks, errorOutGateIt,
              G::deriv);
    ele_mul(preOutGateActBegin, preOutGateActBegin + numBlocks, errorOutGateIt,
            0, dervOutGateAct.slice().begin());
    ele_mul(outputErrorBegin, outputErrorBegin + numBlocks,
            dervOutGateAct.slice().begin(), 0, errorOutGateIt);

    // cell pds (dE/dState)
    transform(preOutGateActBegin, preOutGateActBegin + numBlocks,
              dervOutGateAct.slice().begin(), CO::deriv); // 253
    ele_mul(outGateActBegin, outGateActBegin + numBlocks,
            dervOutGateAct.slice().begin(), 0,
            dervTempCell.slice().begin()); // 253
    ele_mul(outputErrorBegin, outputErrorBegin + numBlocks,
            dervTempCell.slice().begin(), 0, cellErrorBegin); // 253
    ele_mul(errorOutGateIt, errorOutGateIt + numBlocks, peepOutputWtIt, 1,
            cellErrorBegin); // 258

    const View<real_t> &nextErrs = nextErrors[0];
    const real_t *nextErrorCellGateActIt = nextErrs.begin();
    const real_t *nextErrorForgetGateIt =
        nextErrorCellGateActIt + 2 * numBlocks;
    const real_t *nextErrorInputGateIt = nextErrorCellGateActIt + 3 * numBlocks;
    if (nextErrs.begin()) {
      ele_mul(nextErrorForgetGateIt, nextErrorForgetGateIt + numBlocks,
              peepForgetWtIt, 1, cellErrorBegin);
      ele_mul(nextErrorInputGateIt, nextErrorInputGateIt + numBlocks,
              peepInputWtIt, 1, cellErrorBegin);
      ele_mul(nextCellErrors[0].begin(), nextCellErrors[0].end(),
              nextFgActs[0].begin(), 1, cellErrorBegin);
    }

    // input gate error
    transform(inGateActBegin, inGateActBegin + numBlocks, errorInputGateIt,
              G::deriv);
    ele_mul(preGateStateBegin, preGateStateBegin + numBlocks, errorInputGateIt,
            0, dervOutGateAct.slice().begin());
    ele_mul(cellErrorBegin, cellErrorBegin + numBlocks,
            dervOutGateAct.slice().begin(), 0, errorInputGateIt);

    // forget gate error
    LOOP(int d, span(this->num_seq_dims())) {
      const View<real_t> &os = oldStates[d];
      if (os.begin()) {
        transform(forgetGateActBegin, forgetGateActBegin + numBlocks,
                  errorForgetGateIt, G::deriv);
        ele_mul(os.begin(), os.begin() + numBlocks, errorForgetGateIt, 0,
                dervOutGateAct.slice().begin());
        ele_mul(cellErrorBegin, cellErrorBegin + numBlocks,
                dervOutGateAct.slice().begin(), 0, errorForgetGateIt);
      } else {
        memset(errorForgetGateIt, 0, sizeof(real_t) * numBlocks);
      }
    }
    // cell errors
    transform(preGateStateBegin, preGateStateBegin + numBlocks,
              errorCellGateActIt, CI::deriv);
    ele_mul(inGateActBegin, inGateActBegin + numBlocks, errorCellGateActIt, 0,
            dervOutGateAct.slice().begin());
    ele_mul(cellErrorBegin, cellErrorBegin + numBlocks,
            dervOutGateAct.slice().begin(), 0, errorCellGateActIt);

    // constrain errors to be in [-10,10] for stability
    if (!runningGradTest) {
      bound_range(inErrs, -10.0, 10.0);
    }
  }
  void update_derivs(const vector<int> &coords) {
    const real_t *stateBegin = states[coords].begin();
    const real_t *errorCellGateActIt = this->inputErrors[coords].begin();
    const real_t *errorOutGateIt = errorCellGateActIt + numBlocks;
    const real_t *errorForgetGateIt = errorCellGateActIt + 2 * numBlocks;
    const real_t *errorInputGateIt = errorCellGateActIt + 3 * numBlocks;

    real_t *pdInputIt =
        WeightContainer::instance().get_derivs(peepRange).begin();
    real_t *pdOutputIt = pdInputIt + numBlocks;
    real_t *pdForgetIt = pdInputIt + 2 * numBlocks;
    LOOP(int d, span(this->num_seq_dims())) {
      oldStates[d] =
          states.at(range_plus(delayedCoords, coords, stateDelays[d]));
    }
    ele_mul(errorOutGateIt, errorOutGateIt + numBlocks, stateBegin, 1,
            pdOutputIt);
    const View<real_t> &os = oldStates[0];
    if (os.begin()) {
      ele_mul(errorInputGateIt, errorInputGateIt + numBlocks, os.begin(), 1,
              pdInputIt);
      ele_mul(errorForgetGateIt, errorForgetGateIt + numBlocks, os.begin(), 1,
              pdForgetIt);
    }
  }
};

#endif
