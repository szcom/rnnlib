#!/usr/bin/env python
import netcdf_helpers
from optparse import OptionParser
import sys
from PIL import Image
from numpy import *
import re

inputMean = 226.8663
inputStd = 81.62196

#command line options
parser = OptionParser()
parser.add_option("-l", "--lookupfile", action="store", type="string", dest="lookupFile", default="latin_arabic_character_lookup.txt", help="latin/arabic lookup filename")
parser.add_option("-p", "--primarychars", action="store_true", dest="primaryChars", default=False, help="use primary characters only?")
parser.add_option("-u", "--unicode", action="store_true", dest="unicode", default=False, help="use unicode characters from town names?")
parser.add_option("-w", "--wholewords", action="store_true", dest="wholeWords", default=False, help="whole word targets?")
parser.add_option("-d", "--dictfile", action="store", type="string", dest="dictFile", default="latin_dict.txt", help="dict file (for whole word targets)")
parser.add_option("-b", "--backwards", action="store_true", dest="backwards", default=False, help="feed in targets backwards?")


#constants
ligatureString = 'llL'

#parse command line options
(options, args) = parser.parse_args()
if (len(args) < 2):
	print "usage: -options input_filename output_filename"
	print options
	sys.exit(2)

inputFilename = args[0]
outputFilename = args[1]
print options
print "input filename", inputFilename
print "output filename", outputFilename

def convertToPrimaries (labelString):
	return lab.replace('A', ' ').replace('B', ' ').replace('E', ' ').replace('M', ' ').strip().split(' ')

if options.unicode:
	labels = set()
elif options.wholeWords:
	labels = set()
	for l in file(options.dictFile).readlines():
		if l <> "":
			labels.add(l.split()[0])
else:
	#read in labels
	labels = []
	for l in file(options.lookupFile).readlines():
		if l <> "":
	#		print l
			lab = l.split('*')[1].strip()
			if (options.primaryChars):
				for pl in convertToPrimaries(lab):
					if pl not in labels:
						labels.append(pl)
			else:
				labels.append(lab)
if len(labels):
	print len(labels),'labels'
	print labels

#read in input files
filenames = file(inputFilename).readlines()
seqTags = []
seqDims = []
targetStrings = []
wordTargetStrings = []
seqLengths = []
for f in filenames:
	fname = f.strip()
	if fname <> "":
		#read in image dimensions
		print "reading image dimensions", fname
		image = Image.open(fname)
		dims = (image.size[1], image.size[0])
		seqLengths.append(dims[0] * dims[1])
		seqTags.append(fname)
		seqDims.append(dims)
		print image.size
		
		#read in transcription
		truFilename = fname.replace('tif','tru')
		
		try: 
			truFile = open(truFilename)
			print "reading tru file", truFilename
			for l in file(truFilename).readlines():
				words = l.split()
				if words[0] == 'LBL:':
					data = words[1].split(';')
					
					#word target string is the city postcode
					print data[0]
					wordTarget = data[0].split(':')[1]
					wordTargetStrings.append(wordTarget)
					targetString = ""
					if options.wholeWords:
						targetString = wordTarget
					elif options.unicode:
						#[::-1] MEANS REVERSE THE WORDS (arabic written left to right)
						transcript= data[1].split(':')[1].decode("cp1256")
						if not(options.backwards):
							transcript = transcript[::-1]
						transcriptNumFlip = ""
						oldStart = 0
						for match in re.finditer("[0-9]{2,}", transcript):
							transcriptNumFlip += transcript[oldStart:match.start()] + transcript[match.start():match.end()][::-1]
							oldStart = match.start()
						if len(transcriptNumFlip):
							transcriptNumFlip += transcript[len(transcriptNumFlip):]
							transcript = transcriptNumFlip
						print transcript
						for w in transcript:
							asciiStr = w.encode('ascii', "backslashreplace")
							targetString += asciiStr + ' '
							labels.add(asciiStr)
	
					else:
						#[::-1] MEANS REVERSE THE WORDS (arabic written left to right)
						transcript= data[2].split(':')[1].split('|')[:-1]
                                                if not(options.backwards):
                                                        transcript = transcript[::-1]
						for w in transcript:
							ligChar = w.find(ligatureString)
							if ligChar > -1:
								if ligChar == 0:
									lab = w[len(ligatureString):]
								else:
									lab = w[:ligChar]
							else:
								lab = w
							lab = lab[0] + lab[1:].replace('1','').replace('2','')
							#if lab[-1] == '1' or lab[-1] == '2':
								#lab = lab[:-1]
							if options.primaryChars:
								for pl in convertToPrimaries(lab):
									targetString += pl + ' '
							else:				
								targetString += lab + ' '
						
					targetString = targetString.strip()
					print targetString
					targetStrings.append(targetString)
					break
		except IOError:	
			print "tru file", truFilename, "not found, appending empty transcriptions"
			targetStrings.append(labels[0])
			wordTargetStrings.append("-1")
			
if options.unicode:
	labels = list(labels)
	print len(labels),'labels'
	print labels
			
totalLen = sum(seqLengths)
print 'totalLen', totalLen
inputs = zeros((totalLen,1), 'f')
offset = 0
for filename in seqTags:
	print "reading image file", filename
	image = Image.open(filename).transpose(Image.FLIP_TOP_BOTTOM).transpose(Image.ROTATE_270)
	for i in image.getdata():
		inputs[offset][0] = (float(i) - inputMean)/inputStd
		offset += 1

#create a new .nc file
file = netcdf_helpers.NetCDFFile(outputFilename, 'w')

#create the dimensions
netcdf_helpers.createNcDim(file,'numSeqs',len(seqLengths))
netcdf_helpers.createNcDim(file,'numTimesteps',len(inputs))
netcdf_helpers.createNcDim(file,'inputPattSize',len(inputs[0]))
netcdf_helpers.createNcDim(file,'numDims',2)
netcdf_helpers.createNcDim(file,'numLabels',len(labels))

#create the variables
netcdf_helpers.createNcStrings(file,'seqTags',seqTags,('numSeqs','maxSeqTagLength'),'sequence tags')
netcdf_helpers.createNcStrings(file,'labels',labels,('numLabels','maxLabelLength'),'labels')
netcdf_helpers.createNcStrings(file,'wordTargetStrings',wordTargetStrings,('numSeqs','maxWordTargStringLength'),'target strings')
netcdf_helpers.createNcStrings(file,'targetStrings',targetStrings,('numSeqs','maxTargStringLength'),'target strings')
netcdf_helpers.createNcVar(file,'seqLengths',seqLengths,'i',('numSeqs',),'sequence lengths')
netcdf_helpers.createNcVar(file,'seqDims',seqDims,'i',('numSeqs','numDims'),'sequence dimensions')
netcdf_helpers.createNcVar(file,'inputs',inputs,'f',('numTimesteps','inputPattSize'),'input patterns')

#write the data to disk
print "writing data to", outputFilename
file.close()

