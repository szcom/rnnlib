#!/usr/bin/env python

import netcdf_helpers
from scipy import *
from optparse import OptionParser
import sys
import os
from xml.dom.minidom import parse

labels = ['*', '0', '1', '2', '6', '7', '8', '9', 'V', '\\u0621', '\\u0622', '\\u0623', '\\u0625', '\\u0626', '\\u0627', '\\u0628', '\\u0629', '\\u062a', '\\u062b', '\\u062c', '\\u062d', '\\u062e', '\\u062f', '\\u0630', '\\u0631', '\\u0632', '\\u0633', '\\u0634', '\\u0635', '\\u0636', '\\u0637', '\\u0638', '\\u0639', '\\u063a', '\\u0641', '\\u0642', '\\u0643', '\\u0644', '\\u0645', '\\u0646', '\\u0647', '\\u0648', '\\u0649', '\\u064a', '\\u0651']
inputMeans = array([511.8343, 102.7592, 0.03199977])
inputStds = array([134.0498, 24.34094, 0.1758981])


#command line options
parser = OptionParser()

#parse command line options
(options, args) = parser.parse_args()
if (len(args)<2):
	print "usage: -options input_filename output_filename"
	print options
	sys.exit(2)

inputFilename = args [0]
ncFilename = args[1]
print options
print "input filename", inputFilename
print "data filename", ncFilename
seqDims = []
seqLengths = []
targetStrings = []
wordTargetStrings = []
seqTags = []
inputs = []
print "reading data files"
for l in file(inputFilename).readlines():
	inkmlfile = l.strip()
	if len(inkmlfile):
		print inkmlfile
		seqTags.append(inkmlfile)
		upxfile = inkmlfile.replace('inkml', 'upx')
		if os.path.exists(upxfile):
			word = parse(upxfile).getElementsByTagName('hwData')[0].getElementsByTagName('label')[0].getElementsByTagName('alternate')[0].getAttribute('value').strip().replace(' ','*')
			print word
			wts = word.encode('unicode_escape')
			print wts
			wordTargetStrings.append(wts)	
			ts = ""
			for c in word:
				label = c.encode('unicode_escape')
				ts += label + ' '
			ts = ts.strip()
			print ts
			targetStrings.append(ts)
		else:
			wordTargetStrings.append(' ')
			targetStrings.append(' ')			
		oldlen = len(inputs)
		for trace in parse(inkmlfile).getElementsByTagName('trace'):		
			for coords in trace.firstChild.nodeValue.split(','):
				pt = coords.split()
				inputs.append([float(pt[0]), float(pt[1]), 0.0])
			inputs[-1][-1] = 1
		seqLengths.append(len(inputs) - oldlen)
		seqDims.append([seqLengths[-1]])
inputs = ((array(inputs)-inputMeans)/inputStds).tolist()
print len(labels), labels
print labels

#create a new .nc file
file = netcdf_helpers.NetCDFFile(ncFilename, 'w')

#create the dimensions
netcdf_helpers.createNcDim(file,'numSeqs',len(seqLengths))
netcdf_helpers.createNcDim(file,'numTimesteps',len(inputs))
netcdf_helpers.createNcDim(file,'inputPattSize',len(inputs[0]))
netcdf_helpers.createNcDim(file,'numDims',1)
netcdf_helpers.createNcDim(file,'numLabels',len(labels))

#create the variables
netcdf_helpers.createNcStrings(file,'seqTags',seqTags,('numSeqs','maxSeqTagLength'),'sequence tags')
netcdf_helpers.createNcStrings(file,'labels',labels,('numLabels','maxLabelLength'),'labels')
netcdf_helpers.createNcStrings(file,'targetStrings',targetStrings,('numSeqs','maxTargStringLength'),'target strings')
netcdf_helpers.createNcStrings(file,'wordTargetStrings',wordTargetStrings,('numSeqs','maxWordTargStringLength'),'word target strings')
netcdf_helpers.createNcVar(file,'seqLengths',seqLengths,'i',('numSeqs',),'sequence lengths')
netcdf_helpers.createNcVar(file,'seqDims',seqDims,'i',('numSeqs','numDims'),'sequence dimensions')
netcdf_helpers.createNcVar(file,'inputs',inputs,'f',('numTimesteps','inputPattSize'),'input patterns')

#write the data to disk
print "closing file", ncFilename
file.close()
