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
usageHelp="Usage: ${0##*/} [options] save_file data_file(train|test|val|netcdf_file) seq_number"
filenumHelp="-f data file number (index into file list in config, starting at 1, default 1)"
distortionHelp="-d apply input distortions? (default false)"
argsHelp="-a argument string to pass to rnnlib (default '')"
badOptionHelp="Option not recognised"
printHelpAndExit()
{
	echo "$usageHelp"
	echo "$filenumHelp"
	echo "$distortionHelp"
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
distortions=false
arg_string=""
while getopts "hdf:a:" option
do
	case "$option" in
		h) printHelpAndExit 0;;
		d) distortions=true;;
		f) file_num="$OPTARG";;
		a) arg_string="$OPTARG";;
		[?]) printErrorHelpAndExit "$badOptionHelp";;
	esac
done
shift $((OPTIND-1))
if (( $# == 3 ))
then
	save_file=$1
	data_file=$2
	seq_num=$3
	if [ "${data_file}" = "train" -o "${data_file}" = "test" -o "${data_file}" = "val" ]
	then
		data_file=`grep ${data_file}File ${save_file} | cut -d ' ' -f 2 | cut -d ',' -f ${file_num}`
	fi
	output_dir=display_${save_file}_${data_file##*/}_${seq_num}/
	mkdir ${output_dir}
	set -x
	`echo rnnlib --trainFile='""'\
		--valFile='""'\
		--testFile="${data_file}"\
		--display=true\
		--dumpPath="${output_dir}"\
		--autosave=false\
		--verbose=true\
		--testDistortions="${distortions}"\
		--dataset=test\
		--sequence="${seq_num}"\
		"${arg_string}"\
		"${save_file}"`
else
	printHelpAndExit 1
fi
