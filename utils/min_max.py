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

#! /usr/bin/env python
import glob
from optparse import OptionParser

# parse command line options
usage = "usage: %prog input-file"
parser = OptionParser(usage)
(options, args) = parser.parse_args()
#if len(args) != 1:
#        parser.error("incorrect number of arguments")

filePattern = args[0]
#print "input file pattern", filePattern

# load data file
filenames = glob.glob(filePattern)
#print filenames
minVal = 100000000.0
maxVal = -minVal
for f in filenames:
#	print f
	lines = file(f).readlines()
	for l in lines:
		data = l.split()
		for d in data:
			f = float(d)
			if f > maxVal:
				maxVal = f
			if f < minVal:
				minVal = f

print minVal,maxVal
