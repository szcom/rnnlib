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

labstr='LABELS: '
for f in $*; do 
	if [ `grep -c LABELS $f` = '0' ]
	then
		n=$(( `wc -l $f | cut -d ' ' -f 1` - 1 ))
		for i in `seq $n`; do
			labstr=$labstr$f"_"$i" "
			echo $labstr
		done
	else
		str=`grep "LABELS" $f`
		str=${str#"LABELS: "}
		for s in $str; do
			labstr=$labstr$f"_"$s" "
		done
#		echo $str
#		labstr=$labstr${str#"LABELS: "}
	fi
done
echo $labstr > PLOT_MULTI_TEMP
grep DIMENSIONS $1 >> PLOT_MULTI_TEMP
for f in $*; do 
	egrep -v 'LABELS|DIMENSIONS' $f >> PLOT_MULTI_TEMP
done
plot_variables.py -k PLOT_MULTI_TEMP
#rm PLOT_MULTI_TEMP
