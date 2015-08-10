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
import sys
from optparse import OptionParser
from pylab import *

parser = OptionParser(usage='Usage: %prog [options] <save_file>')
parser.add_option('-c', '--connections', dest='connections', type='string', default=None, action='store', help='connections to plot (default = all)')
parser.add_option('-w', '--weight-types', dest='weightTypes', default="weights mdlStdDevs", action='store', help='weight types to plot (default: %default)')
parser.add_option('-b', '--bins', dest='bins', default=100, type='int', action='store', help='number of histogram bins (default: %default)')
parser.add_option('-r', '--range', dest='range', type='string', default=None, action='store', help='histogram range (default = [data.min(), data.max()])')
(opt, args) = parser.parse_args()
if len(args) != 1:
	parser.error('incorrect number of arguments')
weightTypes = opt.weightTypes.split()
connections = None
if opt.connections:
	connections = opt.connections.split()
range = None
if opt.range:
	range = [float(f) for f in opt.range.split()]
for l in open(args[0]):
	words = l.split()
	connStr = words[0]
	connName = connStr.split('_')[1:]
	if (not(connections) or len([c for c in connections if c in connStr])) and len(connName) > 1 and connName[-1] in weightTypes:
		figure()		
		hist([float(f) for f in words[2:]], opt.bins, range)
		xlabel('value')
		ylabel('frequency')
		title(' '.join(connName))
show()
		

					
