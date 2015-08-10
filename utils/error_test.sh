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
usageHelp="Usage: ${0##*/} [options] save_file data_file(train|test|val|netcdf_file)"
verboseHelp="-v verbose? (true or false, default true)"
noiseHelp="-n add noise (wt noise, input noise, sampled mdl noise...) during test? (default false)"
dictHelp="-d dictionary file for CTC decoding"
bigramHelp="-g bigram file for CTC decoding"
logHelp="-l user defined log filename (generated from other options by default)"
fixedHelp="-f fixed length of word target strings for CTC decoding (default -1)"
bestHelp="-b store n best tokens during CTC decoding (default 1)"
fracHelp="-r fraction of data to use for test (default 1)"
confusionHelp="-c print confusion matrix?"
unnormedHelp="-u sort words by unnormalised probability?"
argsHelp="-a argument string to pass to rnnlib (default '')"
badOptionHelp="Option not recognised"
printHelpAndExit()
{
	echo "$usageHelp"
	echo "$verboseHelp"
	echo "$noiseHelp"
	echo "$dictHelp"
	echo "$bigramHelp"
	echo "$logHelp"
	echo "$fixedHelp"
	echo "$bestHelp"
	echo "$fracHelp"
	echo "$confusionHelp"
	echo "$unnormedHelp"
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
verbose=true
noise=false
dict_file='""'
bigram_file='""'
log_file=""
fixed_length=1
n_best=1
data_fraction=1
confusion_matrix=false
sort_by_normed_prob=true
arg_string=""
while getopts "hcunv:d:b:f:g:l:r:a:" option
do
	case "$option" in
		h) printHelpAndExit 0;;
		n) noise=true;;
		v) verbose="$OPTARG";;
		d) dict_file="$OPTARG";;
		g) bigram_file="$OPTARG";;
		l) log_file="$OPTARG";;
		f) fixed_length="$OPTARG";;
		b) n_best="$OPTARG";;
		r) data_fraction="$OPTARG";;
		c) confusion_matrix=true;;
		u) sort_by_normed_prob=false;;
		a) arg_string="$OPTARG";;
		[?]) printErrorHelpAndExit "$badOptionHelp";;
	esac
done
shift $((OPTIND-1))
if (( $# == 2 ))
then
	save_file=$1
	data_file=$2
	if [ "${data_file}" = "train" -o "${data_file}" = "test" -o "${data_file}" = "val" ]
	then
		data_file=`grep ${data_file}File ${save_file} | cut -d ' ' -f 2`
	fi
	if [ "${log_file}" = "" ]
	then
		log_file=error_test
		for f in ${save_file} ${data_file} ${dict_file} ${bigram_file}
		do
			if [ "$f" != '""' ]
			then
				log_file=${log_file}-${f##*\/}
			fi
		done
		if [ "${dict_file}" != '""' ]
		then
			log_file=${log_file}-fixed_length_${fixed_length}-${n_best}_best
		fi
		log_file=${log_file}.log
	fi
	`echo rnnlib --trainFile='""'\
		--valFile='""'\
		--testFile="${data_file}"\
		--errorTest=true\
		--verbose="${verbose}"\
		--testDistortions="${noise}"\
		--autosave=false\
		--dataset=test\
		--fixedLength="${fixed_length}"\
		--nBest="${n_best}"\
		--dataFraction="${data_fraction}"\
		--confusionMatrix="${confusion_matrix}"\
		--sortWordsByNormedProb="${sort_by_normed_prob}"\
		--dictionary="${dict_file}"\
		--bigrams="${bigram_file}"\
		"${arg_string}"\
		"${save_file}"` | tee ${log_file}
else
	printHelpAndExit 1
fi
