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
import re

parser = OptionParser(usage='Usage: %prog [options] <save_file>')
parser.add_option('-c', '--connections', dest='connections', type='string', default=None, action='store', help='connections to plot (default = all)')
parser.add_option('-w', '--weight-types', dest='weightTypes', default="weights mdlStdDevs", action='store', help='weight types to plot (default: %default)')
parser.add_option('-f', '--flat', dest='flat', default=False, action='store_true', help='make flat 2d plots (instead of wireframe)? (default: %default)')
parser.add_option('-i', '--input-dims', dest='inputDims', type='string', default='', action='store', help='input dimensions (if not stored in save file)')
parser.add_option('-o', '--output-size', dest='outputSize', type='int', default=0, action='store', help='output size (if not stored in save file)')
(opt, args) = parser.parse_args()
if len(args) != 1:
	parser.error('incorrect number of arguments')
print opt
weightTypes = opt.weightTypes.split()
connections = None
if opt.connections:
	connections = opt.connections.split()
config = args[0]
text = open(config).read().strip()
layerSizes = {"bias":1}
if opt.inputDims == '':
	inputDims = [re.search('(inputSize) (\S+)', text).group(2)]
else:
	inputDims = opt.inputDims.split()
layerSizes["input"] = product([int(i) for i in inputDims])
if opt.outputSize:
	layerSizes["output"] = opt.outputSize
else:
	delimMatch = re.search('(labelDelimiter) (\S+)', text)
	if delimMatch:
		labelDelim = delimMatch.group(2)
		layerSizes["output"] = len(re.search('(targetLabels) (\S+)', text).group(2).split(labelDelim))
hiddenMatch = re.search('(hiddenSize) (\S+)', text)
if hiddenMatch:
	for n,size in enumerate([int(i) for i in hiddenMatch.group(2).split(',')]):
		layerSizes["hidden_" + str(n)] = size
subMatch = re.search('(subsampleSize) (\S+)', text)
if subMatch:
	for n,size in enumerate([int(i) for i in subMatch.group(2).split(',')]):
		layerSizes["subsample_" + str(n)] = size
print layerSizes
for l in text.split('\n'):
	words = l.split()
	connStr = words[0]
	connName = connStr.split('_')[1:]
	connNameStr = '_'.join(connName)
	if (not(connections) or len([c for c in connections if c in connStr])) and len(connName) > 1 and connName[-1] in weightTypes and 'to' in connName:
		vals = words[2:]
		print connNameStr
		toIndex = connName.index('to')
		fromLayer = '_'.join(connName[:min(2, toIndex)])
		toLayer = '_'.join(connName[toIndex+1:min(len(connName)-1,toIndex + 3)])
		if fromLayer in layerSizes and toLayer in layerSizes:
			fromLayerSize = layerSizes[fromLayer]
			toLayerSize = layerSizes[toLayer]
			out = open(config + '_' + connNameStr, 'w')
			if fromLayer == 'input':
				print >> out, 'DIMENSIONS:', ' '.join(inputDims)		
			else:
				print >> out, 'DIMENSIONS:', fromLayerSize
			start = 0
			for n in range(toLayerSize):
				end = start + fromLayerSize
				print >> out, ' '.join(vals[start:end])
				start = end
			out.close()
					
