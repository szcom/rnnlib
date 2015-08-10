#!/bin/bash
. env.sh
./online_delta.py training_set.txt online.nc
./online_delta.py validation_set.txt online_validation.nc
normalise_inputs.sh -c 2 online.nc online_validation.nc
normalise_targets.sh -c 2 online.nc online_validation.nc
