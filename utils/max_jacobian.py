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

#! /usr/bin/env python

from optparse import OptionParser
from scipy import *
from scipy import io

# parse command line options
usage = "usage: %prog [options] jacobian_list_file output_activations_file output_file"
parser = OptionParser(usage)
parser.add_option("-s", "--sum", action="store_true", dest="sum", help="sum over output derivs?")
parser.add_option("-l", "--bylab", action="store_true", dest="bylab", help="calculate score by label?")
parser.add_option("-b", "--blank", action="store_true", dest="blank", help="allow blank?")
parser.add_option("-x", "--softmax", action="store_true", dest="softmax", help="apply softmax?")
parser.add_option("-m", "--max", action="store_true", dest="max", help="show max only?")
options, args = parser.parse_args()

if len(args) != 3:
	parser.error("incorrect number of arguments")
print options
jaclistfile = args[0]
outactsfile = args[1]
outfile = args[2]
print "jacobian list file", jaclistfile
print "output activations file", outactsfile
print "output file", outfile
a = io.read_array(file(outactsfile), lines=[2,-1])
labels = file(outactsfile).readline().split()[1:]
T = shape(a)[1]
maxindices = []
for t in range(T):
	c = list(a[:,t])
	m = c.index(max(c))
	if not options.blank and labels[m] == 'blank':
		c = c[:-1]
		m = c.index(max(c))
	maxindices.append(m)
print maxindices
print len(labels)
print labels
if options.bylab:
	v = zeros((len(labels),T),'f')
else:
	v = zeros((T,T),'f')
for f in file(jaclistfile).readlines():
	a = io.read_array(file(f.strip()), lines=[1,-1])
	output = int(f.split('/')[-2].split('_')[-1])
#	print 'i', i
	for input in range(T):
#		print 'j', j
		col = a[:,input]
		if options.bylab:
			if options.sum:
				v[maxindices[output]][input] += sum([abs(k) for k in col])
			else:
				v[maxindices[output]][input] = sum([abs(k) for k in col])
		else:
			v[output][input] = sum([abs(k) for k in col])	
#		print 'v[i][j]', v[i][j]
if options.softmax:
	for t in range(T):
		col = v[:,t]
		Z = sum([exp(x) for x in col])
		for y in range(len(col)):
			v[y][t] = exp(col[y]) / Z
if options.max:
	for t in range(T):
		c = list(v[:,t])
		i = c.index(max(c))
		v[:,t] = 0
		v[i][t] = 1
print shape(v)
out = file(outfile, 'w')
if options.bylab:
	print >> out,'LABELS:', ' '.join(labels)
print >> out,'DIMENSIONS:', T 
io.write_array(out, v)