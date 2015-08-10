HDF5 version 1.9.178
------------------------------------------------------------------------------

This directory contains the binary (release) distribution of 
HDF5 1.9 that was compiled on;
    Linux 3.13.0-24-generic x86_64, using GNU C 4.8.2. 

It was built with the following options: 
    -- STATIC C/C++/Fortran libraries
    -- SZIP (encoder enabled) and ZLIB
    -- STATIC HDF5 tools

The contents of this directory are:

    COPYING                 - Copyright notice
    README.txt              - This file
    HDF5-1.9.178-Linux.sh    - HDF5 Install Package

Installation
===========================================================================
1. Execute HDF5-1.9.178-Linux.sh
2. Follow prompts
===========================================================================

After Installation
===========================================================================
The compressed examples file HDF5Examples-0.1.1-Source.tar.gz, located in the 
HDF5 install folder, can be built and tested with CMake and the supplied
HDF518_Examples.cmake file. The HDF518_Examples.cmake expects HDF5 to have
been installed in the default location with above compilers.

To test the installation with the examples;
    Create a directory to run the examples.
    Copy HDF5Examples-0.1.1-Source.tar.gz to this directory, do NOT unzip.
    Copy HDF518_Examples.cmake to this directory.
    Edit HDF518_Examples.cmake line 8 to set INSTALLDIR to where HDF5 is installed.
    Execute from this directory: 
        ctest -S HDF518_Examples.cmake,HDF5Examples-0.1.1-Source -C Release -O test.log

When executed, the ctest script will save the results to the log file, test.log, as
indicated by the ctest command. If you wish the to see more build and test information, 
add "-VV" to the ctest command.

For more information see USING_CMake_Examples.txt in the install folder. 
===========================================================================

Documentation for this release can be found at the following URL:
    http://www.hdfgroup.org/HDF5/doc/.

See the HDF5 home page for further details:
    http://hdfgroup.org/HDF5/

Bugs should be reported to help@hdfgroup.org.
