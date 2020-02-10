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

import netCDF4
import numpy as np
from scipy.io.netcdf import netcdf_file


def create_nc_dim(ncfile, name, d):
    print "creating netcdf dimension:", name, d
    ncfile.createDimension(name, d)


# assumes ncfile will be written over (opened with 'w')
def create_nc_var(ncfile, vname, data, vtype, dims, desc):
    print "creating netcdf variable", vname
    nc_var = ncfile.createVariable(vname, vtype, dims)
    nc_var.longname = desc
    if vtype == 'S1':
        np_data = data
    else:
        np_data = np.asarray(data, dtype=nc_var.typecode() + str(nc_var.itemsize()))
    nc_var[:] = np_data


def max_len(strings):
    max_length = 0
    for s in strings:
        length = len(s)
        if length > max_length:
            max_length = length
    return max_length


def create_nc_strings(ncfile, vname, strings, dims, desc):
    str_length = max_len(strings)

    chars = np.zeros((len(strings), str_length), dtype='S1')
    for i, string in enumerate(strings):
        chars[i] = netCDF4.stringtoarr(string, str_length, 'S')

    create_nc_dim(ncfile, dims[1], str_length)
    create_nc_var(ncfile, vname, np.array(chars), 'S1', dims, desc)
