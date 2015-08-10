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
parser.add_option('-c', '--connections', dest='connections', type='string', default="", action='store', help='connections to plot (default: %default = all)')
parser.add_option('-w', '--weight-types', dest='weightTypes', default='weights mdlStdDevs', action='store', help='weight types to plot (default: %default)')
parser.add_option('-i', '--input-size', dest='inputSize', type='int', default=0, action='store', help='input size (if not stored in save file)')
(opt, args) = parser.parse_args()
if len(args) != 1:
	parser.error('incorrect number of arguments')
print 'options:',opt
weightTypes = opt.weightTypes.split()
if ',' in opt.connections:
	connections = opt.connections.split(',')
else:
	connections = opt.connections.split()
config = args[0]
text = open(config).read()
layerOutputSizes = {'bias':1}
hiddenSizes = re.search('(hiddenSize) (\S+)', text).group(2).split(',')
if opt.inputSize:
	layerOutputSizes['input'] = opt.inputSize
else:
	inputMatch = re.search('(inputSize) (\S+)', text)
	if inputMatch:
		layerOutputSizes['input'] = int(inputMatch.group(2))
for n,size in enumerate([int(i) for i in hiddenSizes]):
	layerName = 'hidden_' + str(n)
	layerOutputSizes[layerName] = size
subsampleMatch = re.search('(subsampleSize) (\S+)', text)
if subsampleMatch:
	for n,size in enumerate([int(i) for i in subsampleMatch.group(2).split(',')]):
		layerOutputSizes['subsample_' + str(n)	] = size
print 'layer output sizes:', layerOutputSizes
for l in text.strip().split('\n'):
	words = l.split()
	connStr = words[0]
	connName = connStr.split('_')[1:]
	if (not len(connections) or len([c for c in connections if c in connStr])) and len(connName) > 1 and connName[-1] in weightTypes and 'to' in connName:
		size = int(words[1])
		vals = words[2:]
		toIndex = connName.index('to')
		fromLayer = '_'.join(connName[:min(2, toIndex)])
		print 'dunping',connStr+', size',size
		if fromLayer in layerOutputSizes:
			toLayer = '_'.join(connName[toIndex+1:min(len(connName)-1,toIndex + 3)])
			fromLayerSize = layerOutputSizes[fromLayer]
			toLayerSize = size / fromLayerSize
			assert(size % fromLayerSize == 0)
			out = open(config + '_' + connStr, 'w')
			print >> out, 'DIMENSIONS:', toLayerSize, fromLayerSize 
			print >> out, ' '.join(vals)
			out.close()
		else:
			print 'layer "'+fromLayer+'" output size unknown, ignoring'
