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
usageHelp="Usage: ${0##*/} [options] save_file"
keepHelp="-k keep temp file? (default false)"
verboseHelp="-v verbose output? (default false)"
hiddenHelp="-u number of hidden units (default 1)"
sigHelp="-f number of significant figures (default 6)"
seqHelp="-s data sequence number for test (default 0)"
pertHelp="-p weight perturbation size (default 1e-5)"
rangeHelp="-r initial weight range (default 0.1)"
codeHelp="-c number of code units (default 1)"
secondHelp="-2 calculate second derivatives? (default false)"
argsHelp="-a argument string to pass to rnnlib (default '')"
badOptionHelp="Option not recognised"
printHelpAndExit()
{
	echo "$usageHelp"
	echo "$keepHelp"
	echo "$verboseHelp"
	echo "$hiddenHelp"
	echo "$sigHelp"
	echo "$seqHelp"
	echo "$pertHelp"
	echo "$rangeHelp"
	echo "$codeHelp"
	echo "$secondHelp"
	echo "$argsHelp"
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
file_num=1
perturbation=1e-5
sig_figs=6
hidden_units=1
keep_temp_file=0
code_units=1
verbose=false
arg_string=""
second_derivs=false
sequence=0
weight_range=0.1
while getopts "hkv2p:s:f:u:c:a:r:" option
do
	case "$option" in
		h) printHelpAndExit 0;;
		k) keep_temp_file=1;;
		v) verbose=true;;
		p) perturbation="$OPTARG";;
		f) sig_figs="$OPTARG";;
		s) sequence="$OPTARG";;
		u) hidden_units="$OPTARG";;
		c) code_units="$OPTARG";;
		2) second_derivs=true;;
		a) arg_string="$OPTARG";;
		r) weight_range="$OPTARG";;
		[?]) printErrorHelpAndExit "$badOptionHelp";;
	esac
done
shift $((OPTIND-1))
if (( $# == 1 ))
then
	save_file=$1
	tmp_file=${save_file}_GRAD_CHECK_TEMP
	extra_levels=`grep hiddenSize ${save_file} | grep -o "," | wc -l | sed s/\ //g`
	sed "s:codeSize:codeSize ${CODESIZE}:g" ${save_file} |
	sed "s:autosave:autosave false:g" |
	sed "s:sampling:sampling false:g" > ${tmp_file}
	echo "gradCheck true" >> ${tmp_file}
	echo "sequence ${sequence}" >> ${tmp_file}
	echo "sigFigs ${sig_figs}" >> ${tmp_file}
	echo "pert ${perturbation}" >> ${tmp_file}
	echo "initWeightRange ${weight_range}" >> ${tmp_file}
	echo "verbose ${verbose}" >> ${tmp_file}
	if ( ${second_derivs} == "true" )
	then
		echo "secondDerivs true" >> ${tmp_file}
		echo "mdlInitStdDev 1" >> ${tmp_file}
	fi
	echo -n "hiddenSize ${hidden_units}" >> ${tmp_file}
	for ((l=0; l < $extra_levels ; l++)) 
		do echo -n ,${hidden_units} >> ${tmp_file}
	done 
	set -x
	`echo rnnlib "${arg_string}" ${tmp_file}`
	if (( ${keep_temp_file} == 0 ))
	then
		rm ${tmp_file}
	fi
else
	printHelpAndExit 1
fi
