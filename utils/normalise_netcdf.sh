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

#!/bin/bash
usageHelp="Usage: ${0##*/} [options] nc_file_1 [nc_file_2, nc_file_3,...]"
stdDevFileHelp="-s netcdf filename for variable mean and std. dev. (calculated from 1st nc file by default)"
variableHelp="-v variable to normalise (default 'inputs')"
badOptionHelp="Option not recognised"
printHelpAndExit()
{
	echo "$usageHelp"
	echo "$stdDevFileHelp"
	echo "$variableHelp"
	exit $1
}
printErrorHelpAndExit()
{
        echo
        echo "$@"
        echo
        echo
        printHelpAndExit 1
}
mean_dev_file=""
variable=inputs
bool_colomn=""
while getopts "hs:v:c:" option
do
	case "$option" in
		h) printHelpAndExit 0;;
		s) mean_dev_file="$OPTARG";;
		v) variable="$OPTARG";;
		c) bool_colomn="-c $OPTARG";;
		[?]) printErrorHelpAndExit "$badOptionHelp";;
	esac
done
shift $((OPTIND-1))
if (( "$#" > "0" ));
then
	script=normalise_netcdf.py
	base=$1
	adj=${base}_ADJ
	echo ${base}
	echo ${adj}
	mean_dev_file_str=""
	if [ "${mean_dev_file}" != "" ]
	then
		mean_dev_file_str="-s ${mean_dev_file}"
	fi
	$script ${mean_dev_file_str} ${bool_colomn} -i ${variable} -o ${variable} ${base} ${adj}
	ncks -A $adj $base
	rm $adj
	if [ "${mean_dev_file_str}" == "" ]
	then
		mean_dev_file_str="-s ${base}"
	fi
	while [ $# -gt 1 ]
		do
		shift
		base=$1
		adj=${base}_ADJ
		echo ${base}
		echo ${adj}
		$script ${mean_dev_file_str} -i ${variable} -o ${variable} ${base} ${adj}
		ncks -A $adj $base
		rm $adj
		done
else
	printHelpAndExit 1
fi
