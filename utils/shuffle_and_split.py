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

#!/usr/bin/env python
import random
import sys
from scipy import *
from optparse import OptionParser

#command line options
parser = OptionParser("usage: %prog [options] input_file split_fractions output_root")
(options, args) = parser.parse_args()
if (len(args) != 3):
	parser.error("incorrect number of arguments")
print options
infilename = args [0]
splitstrings = args[1].split()
if len(splitstrings) == 1:
	numSplits = int(splitstrings[0])
	splitfracs = [1.0/numSplits] * numSplits
else:
	splitfracs = [float(i) for i in splitstrings]
outroot = args[2]
lines = file(infilename).readlines()
random.shuffle(lines)
if abs(sum(splitfracs) - 1) > 0.000001:
	print "ERROR: split fractions sum to", sum(splitfracs), "not 1.0, exiting"
	sys.exit(0)
#print splitfracs
line = 0
splitlines = []
for i,s in enumerate(splitfracs[:-1]):
	line += int(s * len(lines))
	splitlines.append(line)
splitlines.append(len(lines))
oldl = 0
numDigits = len(str(len(splitlines) - 1))
for i,l in enumerate(splitlines):
	out = file(outroot + '_' + str(i).rjust(numDigits, '0') + '.txt', 'w')
	for line in lines[oldl:l]:
		print >> out, line.strip()
	oldl =l
out.close()
