#!/usr/bin/env python
import netcdf_helpers
from scipy import *
from optparse import OptionParser
import sys
from PIL import Image

padDims = (95, 77)

#command line options
parser = OptionParser()
parser.add_option("-p", "--pad", action="store_true", dest="pad", help="pad image to 95x77 pixels?")
parser.add_option("-d", "--dummy", action="store_true", dest="dummy", help="use dummy labels?")
parser.add_option("-c", "--chars", action="store_true", dest="chars", help="build char dataset?")

#parse command line options
(options, args) = parser.parse_args()
if (len(args)<2):
	print "usage: -options input_filename output_filename"
	print options
	sys.exit(2)

if options.chars:
	labels = [str(x).rjust(2,'0') for x in range(1,35)]
 	inputMean = 244.2604
	inputStd = 30.54056
else:
	labels = [str(x).rjust(2,'0') for x in range(35,47)]
 	inputMean = 247.2425
	inputStd = 29.01598

inputFilename = args[0]
ncFilename = args[1]
print options
print "input filename", inputFilename
print "data filename", ncFilename

seqDims = []
seqLengths = []
targetStrings = []
seqTags = []
filenames = []
print "reading file data"
inputlines = file(inputFilename).readlines()
for line in inputlines:
	fname = line.strip()
	if len(fname):
		print fname
		try:
			im = Image.open(fname)
			if options.dummy:
				label = labels[0]
			else:
				label = fname.split('/')[-1][:2]
			targetStrings.append(label)
			seqTags.append(fname)
			if options.pad:
				dims = (max(padDims[0], im.size[1]), max(padDims[1], im.size[0]))
			else:
				dims = (im.size[1], im.size[0])
			seqLengths.append(product(dims))
			seqDims.append(dims)
			print dims
		except:
			print "could not open image, removing from dataset"
			inputlines.remove(line)	
	else:
		inputlines.remove(line)

print "allocating input array"
inputs = zeros((sum(seqLengths), 1), 'f')
print shape(inputs)
offset = 0
print "writing inputs"
for tag, line in zip(seqTags, inputlines):
	print "reading", tag
	im = Image.open(tag).transpose(Image.FLIP_TOP_BOTTOM).transpose(Image.ROTATE_270).convert('L')
	if options.pad and (im.size[0] < padDims[0] or im.size[1] < padDims[1]):
		w = max(padDims[0], im.size[0])
		h = max(padDims[1], im.size[1])
		bigIm = Image.new(im.mode, (w,h), 255)
		bigIm.paste(im, ((w - im.size[0])/2, (h - im.size[1])/2))
		im = bigIm
	for n in im.getdata():
		inputs[offset][0] = (float(n) - inputMean)/inputStd
		offset += 1
		
print len(labels), "labels:"
print labels

#create a new .nc file
file = netcdf_helpers.NetCDFFile(ncFilename, 'w')

#create the dimensions
netcdf_helpers.createNcDim(file,'numSeqs',len(seqLengths))
netcdf_helpers.createNcDim(file,'numTimesteps',len(inputs))
netcdf_helpers.createNcDim(file,'inputPattSize',len(inputs[0]))
netcdf_helpers.createNcDim(file,'numDims',2)
netcdf_helpers.createNcDim(file,'numLabels',len(labels))

#create the variables
netcdf_helpers.createNcStrings(file,'seqTags',seqTags,('numSeqs','maxSeqTagLength'),'sequence tags')
netcdf_helpers.createNcStrings(file,'labels',labels,('numLabels','maxLabelLength'),'labels')
netcdf_helpers.createNcStrings(file,'targetStrings',targetStrings,('numSeqs','maxTargStringLength'),'target strings')
netcdf_helpers.createNcVar(file,'seqLengths',seqLengths,'i',('numSeqs',),'sequence lengths')
netcdf_helpers.createNcVar(file,'seqDims',seqDims,'i',('numSeqs','numDims'),'sequence dimensions')
netcdf_helpers.createNcVar(file,'inputs',inputs,'f',('numTimesteps','inputPattSize'),'input patterns')

#write the data to disk
print "closing file", ncFilename
file.close()

