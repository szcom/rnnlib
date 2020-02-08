# Copyright 2009,2010 Alex Graves
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

# from Scientific.IO.NetCDF import NetCDFFile
import netCDF4
from scipy.io.netcdf import netcdf_file
import numpy as np


def createNcDim(ncfile, name, d):
    print "creating netcdf dimension:", name, d
    ncfile.createDimension(name, d)


# assumes ncfile will be written over (opened with 'w')
def createNcVar(ncfile, vname, data, vtype, dims, desc):
    print "creating netcdf variable", vname
    nc_var = ncfile.createVariable(vname, vtype, dims)
    nc_var.longname = desc
    if vtype == 'S1':
        np_data = data
    else:
        np_data = np.asarray(data, dtype=nc_var.typecode() + str(nc_var.itemsize()))
    assert nc_var.shape == np_data.shape  # TODO:remove
    nc_var[:] = np_data
    print nc_var.shape


def maxLen(strings):
    maxLength = 0
    for s in strings:
        length = len(s)
        if (length > maxLength):
            maxLength = length
    return maxLength


def createNcStrings(ncfile, vname, strings, dims, desc):
    str_length = maxLen(strings)

    chars = np.empty((len(strings), str_length), dtype='S' + str(str_length))
    [np.append(chars, netCDF4.stringtoarr(string, str_length, 'S')) for string in strings]

    createNcDim(ncfile, dims[1], str_length)
    createNcVar(ncfile, vname, np.array(chars), 'S1', dims, desc)

# def	createNcString(ncfile,vname,string,dims,desc):
# 	print "writing string",vname
# 	nullString = string + '\0'
# 	createNcDim(ncfile,dims[0],len(nullString))
# 	createNcVar(ncfile,vname,nullString,'c',dims,desc)
