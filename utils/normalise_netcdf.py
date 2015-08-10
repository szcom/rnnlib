#!/usr/bin/env python
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
import netcdf_helpers
import sys
from numpy import *
from optparse import OptionParser

parser = OptionParser("usage: %prog input_filename output_filename")
parser.add_option("-m", "--maxarraysize", action="store", type="int", dest="maxArraySize", default=8000000, help="maximum array size for std and mean calcs")
parser.add_option("-i", "--inputarrayname", action="store", type="string", dest="inputArrayName", default="inputs", help="name of input array")
parser.add_option("-o", "--outputarrayname", action="store", type="string", dest="outputArrayName", default="inputs", help="name of output array")
parser.add_option("-s", "--stdmeanfilename", action="store", type="string", dest="stdMeanFilename", default="", help="file to use for stds and means")
parser.add_option("-b", "--bigfile", action="store_true", dest="bigFile", default=False, help="use memory optimisations for big files? (slower)")
parser.add_option("-c", "--booleanColomn", action="store", type="int", dest="booleanColomn", default=-1, help="dont normalize nth colomn of input array")

def Std(array,axis):
	if shape(array)[axis]>1:
		return (std(array,axis))
	return array

#parse command line options
(options, args) = parser.parse_args()
print options
if (len(args) != 2):
	parser.error("incorrect number of arguments")
inputFilename = args[0]
outputFilename = args[1]
print 'inputFilename', inputFilename
infile = netcdf_helpers.NetCDFFile(inputFilename, 'r')

print "loading in input array"
inputVar = infile.variables[options.inputArrayName]
outputArray = zeros(inputVar.shape, 'f')
if options.bigFile:
	offset = 0
	step = options.maxArraySize
	while offset < inputVar.shape[0]:
		max = min (offset+step, inputVar.shape[0])
		outputArray[offset:max] = inputVar[offset:max]
		offset += step
else:
	outputArray = inputVar.getValue()

if options.stdMeanFilename <> "":
	print "reading std deviations and means from",options.stdMeanFilename
	stdMeanFile = netcdf_helpers.NetCDFFile(options.stdMeanFilename, 'r')
	inputStds = array(stdMeanFile.variables[options.inputArrayName+'Stds'].getValue())
	inputMeans = array(stdMeanFile.variables[options.inputArrayName+'Means'].getValue())

else:
	print "calculating std deviations"
	inputStds=Std(outputArray[:options.maxArraySize],0)
	print "calculating means"
	inputMeans=mean(outputArray[:options.maxArraySize],0)
if options.booleanColomn > 0:
        print "dont normalize boolean colomn", options.booleanColomn
        inputStds[options.booleanColomn] = 1
        inputMeans[options.booleanColomn] = 0
print inputStds
print inputMeans

for p in range(len(inputStds)):
	if (inputStds[p]>0):
		if options.bigFile:
			offset = 0
			step = options.maxArraySize
			while offset < len(outputArray):
				max = min (offset+step, len(outputArray))
				outputArray[offset:max,p] = (outputArray[offset:max,p] - inputMeans[p])/inputStds[p]
				offset += step
		else:
			outputArray[:,p]=(outputArray[:,p]-inputMeans[p])/inputStds[p]

outfile = netcdf_helpers.NetCDFFile(outputFilename, 'w')

for d in inputVar.dimensions:
	netcdf_helpers.createNcDim(outfile,d,infile.dimensions[d])

if options.stdMeanFilename == "":
	netcdf_helpers.createNcVar(outfile,options.outputArrayName+'Means',inputMeans,'f',(inputVar.dimensions[1],),'input means')
	netcdf_helpers.createNcVar(outfile,options.outputArrayName+'Stds',inputStds,'f',(inputVar.dimensions[1],),'input std deviations')
netcdf_helpers.createNcVar(outfile,options.outputArrayName,outputArray,'f',inputVar.dimensions,options.inputArrayName+' adjusted for mean 0 and std dev 1')
outfile.close()
