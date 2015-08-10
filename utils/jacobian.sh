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
usageHelp="Usage: ${0##*/} [options] save_file data_file(train|test|val|netcdf_file) seq_number jacobian_coordinates"
#keepHelp="-k keep temp file?"
filenumHelp="-f data file number (index into file list in config, starting at 1, default 1)"
distortionHelp="-d apply input distortions? (default false)"
badOptionHelp="Option not recognised"
printHelpAndExit()
{
	echo "$usageHelp"
#	echo "$keepHelp"
	echo "$filenumHelp"
	echo "$distortionHelp"
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
#keep_temp_file=0
#while getopts "hkdf:" option
while getopts "hdf:" option
do
	case "$option" in
		h) printHelpAndExit 0;;
#		k) keep_temp_file=1;;
		d) distortions=true;;
		f) file_num="$OPTARG";;
		[?]) printErrorHelpAndExit "$badOptionHelp";;
	esac
done
shift $((OPTIND-1))
if (( $# == 4 ))
then
	save_file=$1
	data_file=$2
	seq_num=$3
	coords=$4
	if [ "${data_file}" = "train" -o "${data_file}" = "test" -o "${data_file}" = "val" ]
	then
		data_file=`grep ${data_file}File ${save_file} | cut -d ' ' -f 2 | cut -d ',' -f ${file_num}`
	fi
	output_dir=jacobian_${save_file}_${data_file##*/}_${seq_num}_${coords}/
	mkdir ${output_dir}
	rnnlib --trainFile='""'\
		--valFile='""'\
		--testFile="${data_file}"\
		--jacobianCoords=${coords} \
		--dumpPath="${output_dir}"\
		--autosave=false\
		--verbose=true\
		--testDistortions="${distortions}"\
		--dataset=test\
		"${save_file}" 
else
	printHelpAndExit 1
fi

