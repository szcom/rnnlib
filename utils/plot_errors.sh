#Copyright 2009,2010 Alex Graves
#
# This file is part of RNNLIB.
# 
# RNNLIB is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# RNNLIB is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with RNNLIB.  If not, see <http://www.gnu.org/licenses/>.

#! /bin/bash

UTILS=/home/alex/code/neural_net_console-1.0/utils
PL=plot_errors.py
if [ $# = 4 ]
then
	FILE=$1_$2_$3_$4
	grep -A $4 "$2 errors" $1 | grep $3 | cut -f2 -d" " > $FILE
	$PL $FILE
#	rm $FILE
else
	echo "usage: plot_errors.sh save_file data_set(train|test|validation) error_type(labelErrorRate,ctcMlError...) num_error_types"
fi
