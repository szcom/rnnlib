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
#from mpl_toolkits.mplot3d import Axes3D
from optparse import OptionParser
from pylab import *
from numpy import *
from PIL import Image
#import numpy
#from numpy import array
import sys

# parse command line opt
usage = "usage: %prog [opt] input-file"
parser = OptionParser(usage)
parser.add_option("-s", "--style", dest="linestyle", default="None", action="store", type="string", help="Line styles not implemented yet")
parser.add_option("-o", "--output", dest="outputFilename", default="screen", action="store", type="string", help="output filename (screen = plot to screen)")
parser.add_option("-g", "--grid-dimensions", dest="gridDims", default=None, action="store", type="string", help="grid dimensions (gridPlot only; assumed square if None)")
parser.add_option("-a", "--all-grids", dest="allGrids", default=False, action="store_true", help="show all grids? (otherwise limited to 10)")
parser.add_option("--gather-grids", dest="gatherGrids", default=False, action="store_true", help="gather grids onto one plot?")
parser.add_option("-l", "--lo", dest="lo", default=0, action="store", type=float, help="lo value for y axis (auto if hi and lo 0)")
parser.add_option("-i", "--hi", dest="hi", default=0, action="store", type=float, help="hi value for y axis (auto if hi and lo 0)")
parser.add_option("-k", "--key", dest="key", default=False, action="store_true", help="display key")
parser.add_option("--wire", dest="wire", default=False, action="store_true", help="plot 3d wireframe?")
parser.add_option("--use", dest="use", default="", type='string', action="store", help="comma separated list of variables to plot (indices or labels) OVERRIDES ignore")
parser.add_option("--ignore", dest="ignore", default="", type='string', action="store", help="comma separated list of variables not to plot (indices or labels)")
parser.add_option('-p', "--printlabels", dest="printlabels", default=False, action="store_true", help="print labels of variables with highest value?")
parser.add_option("-n", "--noaxes", dest="noAxes", default=False, action="store_true", help="don't show axes")
parser.add_option("-c", "--colour", dest="colour", default=False, action="store_true", help="plot in colour? (gridPlot only)")
parser.add_option("-m", "--min", dest="min", default=0, action="store", type=int, help="min line number")
parser.add_option("-x", "--max", dest="max", default=0, action="store", type=int, help="max line number (all lines if 0)")
parser.add_option("-t", "--transpose", dest="transpose", default=False, action="store_true", help="transpose data?")
parser.add_option("-f", "--flipud", dest="flipud", default=False, action="store_true", help="flip data upside down?")
parser.add_option("-b", "--abs", dest="abs", default=False, action="store_true", help="plot absolute values?")
parser.add_option("-r", "--rotate", dest="rotate", default=False, action="store_true", help="rotate data 90 degrees?")
parser.add_option("--ycropbegin", dest="yCropBegin", default=0, action="store", type=int, help="y crop begin")
parser.add_option("--ycropend", dest="yCropEnd", default=-1, action="store", type=int, help="y crop end")
parser.add_option("--xcropbegin", dest="xCropBegin", default=0, action="store", type=int, help="x crop begin")
parser.add_option("--xcropend", dest="xCropEnd", default=-1, action="store", type=int, help="x crop end")
parser.add_option("-v", "--vectorplot", dest="vectorPlot", default=False, action="store_true", help="plot vector field?")
parser.add_option("--rgb", dest="rgb", default=False, action="store_true", help="combine into rgb image(s)? (false if vectorPlot true)")
parser.add_option("--blockdims", dest="blockDims", default=None, action="store", type="string", help="2d block dims (e.g. 2,2)")
parser.add_option("--rgbmeans", dest="rgbMeans", default="0,0,0", action="store", type="string", help="rgb means")
parser.add_option("--rgbdevs", dest="rgbDevs", default="1,1,1", action="store", type="string", help="rgb means")
parser.add_option("--rgbsplit", dest="rgbSplit", default=False, action="store_true", help="split rgb image into separate channels?")
parser.add_option("--smooth", dest="smooth", default=False, action="store_true", help="plot grids as smooth data rather than images?")
parser.add_option("--log", dest="log", default=False, action="store_true", help="plot data on log scale?")
(opt, args) = parser.parse_args()

if opt.vectorPlot:
	opt.rgb = False
if opt.blockDims <> None:
	opt.allGrids = True

if len(args) != 1:
        parser.error("incorrect number of arguments")
infilename = args[0]

print opt
print "input filename", infilename

#read header (if any)
numRowsToSkip = 0
labels = []
data = file(infilename)
dimensions = []
line = data.readline().split()
gridDims = None
if opt.gridDims:
	gridDims = [int(i) for i in opt.gridDims.split()]
	assert(len(gridDims) == 2)
while line[0][-1] == ':' or line[0] == "#":
	if line[0] == "LABELS:" or line[0] == "#":
		labels = line[1:]
		numRowsToSkip += 1
		print 'labels =', labels
	if line[0] == "DIMENSIONS:":
		for d in line[1:]:
			d = int(d)
			if d > 1:		
				dimensions.append(d)
		numRowsToSkip += 1
		print 'dimensions =', dimensions
		if len(dimensions) >= 2 and not(gridDims):
			gridDims = dimensions[-2:]
	line = data.readline().split()

# load data file
data = loadtxt(infilename, skiprows = numRowsToSkip)
if len(data.shape) == 1:
	data.shape = (1, data.shape[0])
#if len(labels):
#	assert len(labels) == len(data)
if len(labels) == len(data):
	labelledLines = True
else:
	labelledLines = False	

#apply corrections to data depending on opt
useList = opt.use.replace(',', ' ').split()
ignoreList = opt.ignore.replace(',', ' ').split()
if len(ignoreList) and not(len(useList)):
	for i in range(data.shape[0]):
		if str(i) not in ignoreList and not(labelledLines and labels[int(i)] in ignoreList):
			useList.append(i)
if len(useList):
	print data.shape
	lines = set()
	for v in useList:
		if labelledLines and v in labels:
			i = labels.index(v)
		else:
			i = int(v)
		if i >= 0 and i < len(data):
			lines.add(i)
	newlabels = []
	newdata = []
	print 'only plotting variables', sorted(lines)
	for i in sorted(lines):
		if labelledLines:
			newlabels.append(labels[i])
		newdata.append(data[i])
	data = array(newdata)
	if labelledLines:
		labels = newlabels
		print 'with labels', labels
if opt.abs:
	newdata = []
	for l in data:
		newdata.append(fabs(l))
	newdata = array(newdata)
	data = newdata
if opt.log:
	newdata = []
	for l in data:
		newdata.append(log10(fabs(l)+1))
	newdata = array(newdata)
	data = newdata

assert len(data)
print 'data shape =', data.shape

if gridDims and not opt.gatherGrids:
	data.shape = (data.shape[0]*data.shape[1] / product(gridDims), product(gridDims))
if opt.max:
	data = data[opt.min:opt.max]
elif opt.min:
	data = data[opt.min:]

if gridDims and len(data) > 5 and not opt.allGrids:
	data = data[:5]

#print data
def pcolorLine(line, gridDims):
	size = product(gridDims)
	numGrids = len(line) / size
	if not opt.allGrids:
		numGrids = min(numGrids, 5)
	if opt.rgb:
		area = size / 3
	else:
		area = size
	if not(gridDims):
		side = sqrt(area)
		gridDims = [side,side]
	width = gridDims[0]	
	height = gridDims[1]
	print "area",area,"width",width,"height",height
	begin=0
	rowscols = ceil(sqrt(numGrids))
	vectorGrids = []
	for i in range(numGrids):
		#get the data
		end = begin + size
		grid = line[begin:end]
		if opt.abs:
			newgrid = []
			for g in grid:
				newgrid.append(math.fabs(g))
			grid = array(newgrid)
		begin = end
		if (opt.rgb):
			grid.shape = (width,height,3)
		else:
			grid.shape = (width,height)
		if not opt.vectorPlot:
			subplot(rowscols, rowscols, i+1) 
		if not opt.transpose:
			grid = transpose(grid)
		if opt.rotate:
			grid = rot90(grid)
		if opt.flipud:
			grid = flipud(grid)
		cropped = False
		if opt.yCropBegin <> 0 or opt.yCropEnd <> -1:
			grid = grid[opt.yCropBegin: opt.yCropEnd]
			cropped = True
		if opt.xCropBegin <> 0 or opt.xCropEnd <> -1:
			newGrid = []
			for line in grid:
				newGrid.append(line[opt.xCropBegin: opt.xCropEnd])
			grid = array(newGrid)
			cropped = True
		if cropped:
			print 'cropped to', grid.shape
		if opt.vectorPlot:
			vectorGrids.append(grid)
		elif opt.wire:
			ax3d = Axes3D(fig)
			x,y = mgrid[0:grid.shape[0]:1, 0:grid.shape[1]:1]
			ax3d.plot_wireframe(x,y,grid)
		elif opt.smooth:
			grid = flipud(grid)
			pcolor(grid, shading = 'flat')
			setp(gca(),axisbelow='false',frame_on='false',xlim=(0.0,float(grid.shape[1])),ylim=(0.0,float(grid.shape[0])))
		else:
			imshow(grid, interpolation='nearest')

	if opt.vectorPlot:
		return vectorGrids, rowscols
	
if gridDims:

	if not opt.colour:
		gray()

	if opt.vectorPlot:
		fig = figure()
		title(infilename)
		xGrids, rowscols = pcolorLine(data[0], gridDims)
		yGrids, rowscols2 = pcolorLine(data[1], gridDims)
		for n in range(len(xGrids)):
			subplot(rowscols, rowscols, n+1) 
			quiver(xGrids[n], yGrids[n])
			setp(gca(),xticks=[],yticks=[],frame_on=False,xlim=(0.0,float(xGrids[0].shape[0])),ylim=(0.0,float(xGrids[0].shape[1])))
		
	else:
		if opt.rgb:
			rgbDevs = numpy.array(opt.rgbDevs.split(',')).astype(float)
			rgbMeans = numpy.array(opt.rgbMeans.split(',')).astype(float)
			newData = []
			for i in range(0, len(data), 3):
				if i + 3 <= len(data):
					arrays = [numpy.array(data[i]),numpy.array(data[i+1]),numpy.array(data[i+2])]
					for j in range(3):
						arrays[j] = ((arrays[j]*rgbDevs[j]) + rgbMeans[j]) / 255.0
						for k in range(len(arrays[j])):
							if arrays[j][k] < 0.0:
								arrays[j][k] = -arrays[j][k]
							elif arrays[j][k] > 1.0:
								arrays[j][k] = 2.0-arrays[j][k]
					newData.append(numpy.column_stack(arrays).flatten())
			data = newData
		elif opt.rgbSplit:
			r = data[::3]
			g = data[1::3]
			b = data[2::3]
			data = r_[r,g,b]
		if opt.blockDims <> None:
			blockDims = numpy.array(opt.blockDims.split(',')).astype(int)
			print blockDims
			if len(blockDims == 2):
				blockArea = numpy.product(blockDims)
				newData = []
				for i in range(0, len(data), blockArea):
					if i + blockArea <= len(data):
						width = gridDims[0]
						height = len(data[i]) / gridDims[0]
						if (opt.rgb):
							colourDepth = 3
						else:
							colourDepth = 1
						height /= colourDepth
						print "width, height", width, height
						newline = zeros((width*blockDims[0], height*blockDims[1], colourDepth), 'f')
						print newline.shape
						index = 0
						for j in range(width):
							for k in range(height):
								blockIndex = 0
								for l in range(blockDims[0]):
									for m in range(blockDims[1]):
										newline[(j*blockDims[0]) + l][(k*blockDims[1]) + m][:] = data[i + blockIndex][index:index+colourDepth]
										blockIndex += 1
								index += colourDepth
						newData.append(newline.flatten())
				data = newData
				if gridDims:
					gridDims[0] *= blockDims[0]
		for i,line in enumerate(data):
			fig = figure()
			pcolorLine(line, gridDims)
			title(infilename + '_' + str(i + opt.min))
			fig.subplots_adjust(top=0.95, bottom=0.05, right=0.98,left=0.05)

else:

	# create figure
	fig = figure()
	title(infilename)
	axes = axes()

	# add data to figure
	lineLabelMap = dict()
	colors = []
	for c in range(data.shape[0]):
		lineData = data[c,:]
		if labelledLines:
			lab = labels[c]
		else:
			lab = str(c)
		line2d = axes.plot(lineData, linewidth=1.5, label = lab)[0]
		colors.append(line2d.get_color())
		lineLabelMap[line2d] = lab

	if labelledLines and opt.printlabels:
		prevMaxNum = -1
		yPlotCoord = axes.get_ylim()[1] + 0.01 * (axes.get_ylim()[1] - axes.get_ylim()[0])
		for r in range(data.shape[1]):
			max = 0
			maxNum = -1
			for v in range(len(data[:,r])):
				val = abs(data[v][r])
				if val > max:
					max = val
					maxNum = v
			if maxNum < len(labels) -1 and maxNum <> prevMaxNum:
					text(r, yPlotCoord, labels[maxNum], {'color': 'black', 'fontsize' : 'medium'}, fontweight = 'bold', horizontalalignment = 'center', verticalalignment = 'bottom')
					prevMaxNum = maxNum

if (opt.lo <> 0 or  opt.hi <> 0):
	axes.set_ylim( (opt.lo,opt.hi))

if opt.noAxes:
	axis('off')
else:
	axis('on')
if opt.key:
	legend(prop = matplotlib.font_manager.FontProperties(size = 'smaller'))

#def pick(event):
#	if event.key=='p' and event.inaxes is not None:
#		ax = event.inaxes
#		a = ax.pick(event.x, event.y)

#		if isinstance(a, Line2D) and a in lineLabelMap:
#			labelNum = int(lineLabelMap[a])
#			if labelNum >= 0 and labelNum < len(labels):
#				print event.xdata, event.ydata, labels[labelNum], labelNum
#				text(event.xdata, event.ydata, labels[labelNum], {'color': 'black', 'fontsize' : 'medium'}, fontweight = 'bold', horizontalalignment = 'center', verticalalignment = 'center')
#				draw()

# show figure
if opt.outputFilename == "screen":
	#connect('key_press_event', pick)
	show()
else:
	savefig(opt.outputFilename)

