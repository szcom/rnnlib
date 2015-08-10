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
from Scientific.IO.NetCDF import NetCDFFile
from numpy import *
from optparse import OptionParser

parser = OptionParser("usage: %prog netcdf_filename")
parser.add_option("-z", "--zero", action="store", type="float", dest="zero", default=0, help="zero value")
parser.add_option("-o", "--one", action="store", type="float", dest="one", default=1, help="one value")

#parse command line options
(options, args) = parser.parse_args()
print options
if (len(args) != 1):
	parser.error("incorrect number of arguments")
filename = args[0]
print 'filename', filename
infile = NetCDFFile(filename, 'a')
invar = infile.variables['inputs']
inputs = invar.getValue()
print inputs.shape
for i in range(len(inputs)):
	for j in range(len(inputs[0])):
		if inputs[i][j] > 0:
			inputs[i][j] = options.one
		else:
			inputs[i][j] = options.zero			
invar.assignValue(inputs)
infile.close()
