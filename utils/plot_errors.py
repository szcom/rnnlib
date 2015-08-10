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
from pylab import *
import re
from optparse import OptionParser

usage = "usage: %prog log_file"
parser = OptionParser(usage)
parser.add_option("-v", "--verbose", dest="verbose", default=False, action="store_true", help="verbose plot (including error types with verboseChar)?")
parser.add_option("-c", "--verbosechar", dest="verboseChar", default="_", action="store", help="special character for verbose plots")
parser.add_option("-e", "--err-types", dest="errTypes", default="seqRmsError seqMixtureError crossEntropyError classificationError mixtureError sumSquaresError labelError ctcError mdlError", action="store", help="space separated list of error types to plot (empty list => plot all)")
(opt, args) = parser.parse_args()
errors = dict()

#print opt
if len(args) != 1:
        parser.error("incorrect number of arguments")
filename = args[0]
print "plotting errors from", filename
lines = file(filename, 'r').readlines()
errorType = ""
epochNum = -1
bestEpochs = dict()
allowedErrors = opt.errTypes.split()
for l in lines:
	words = l.split()
	if (l.find("epoch") >= 0 and l.find("took") >= 0):
		epochNum = int(l.split()[1])
	elif (epochNum >= 0):
		if (l.find("train errors") >= 0):
			errorType = "train"
		elif (l.find("test errors") >= 0):
			errorType = "test"
		elif (l.find("validation errors") >= 0):
			errorType = "validation"
		elif (len(words) == 0 or l.find("best") >= 0):
			errorType = ""
			if l.find("best network (") >= 0:
				bestEpochs[l.split()[2]] = epochNum
			elif l.find("saving to") >= 0 and l.find(".best_") >= 0:
				bestEpochs['(' + l.split('_')[-1].split('.')[0] + ')'] = epochNum
		elif len(words) == 2 and errorType <> "":
			errWord = words[0]
			if (len(allowedErrors)==0 or errWord in allowedErrors) and (opt.verbose or opt.verboseChar not in errWord):
				errVal = float(words[1].strip('%'))
				if errWord not in errors:
					errors[errWord] = dict()
				if errorType in errors[words[0]]:
					errors[errWord][errorType][0].append(epochNum)
					errors[errWord][errorType][1].append(errVal)
				else:
					errors[errWord][errorType] = [[epochNum],[errVal]]

if (len(errors)):
	for err in errors.items():
		figure()
		title(filename + ' \n' + err[0]) 
		for dataSet in err[1].items():
			plot(dataSet[1][0], dataSet[1][1], linewidth=1.5, label=dataSet[0], marker='+')
		axes = gca()
		yRange = [axis()[2], axis()[3]]
		if len(bestEpochs) > 0:
			bone()
			for best in bestEpochs.items():
				if re.search("\(.*\)", best[0]):
					lab = "best "+best[0]
				else:
					lab = "best network"
				plot([best[1], best[1]], yRange, linestyle ='--', linewidth=1, label=lab)
		legend()
		legend(prop = matplotlib.font_manager.FontProperties(size = 'smaller'))
else:
	print "allowed errors:", allowedErrors
	print "no allowed errors found, exiting"

show()
