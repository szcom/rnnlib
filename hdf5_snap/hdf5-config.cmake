#-----------------------------------------------------------------------------
# HDF5 Config file for compiling against hdf5 build directory
#-----------------------------------------------------------------------------
GET_FILENAME_COMPONENT (SELF_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

#-----------------------------------------------------------------------------
# User Options
#-----------------------------------------------------------------------------
set (HDF5_ENABLE_PARALLEL OFF)
set (HDF5_BUILD_FORTRAN   OFF)
set (HDF5_ENABLE_F2003    )
set (HDF5_BUILD_CPP_LIB   OFF)
set (HDF5_BUILD_TOOLS     OFF)
set (HDF5_BUILD_HL_LIB    ON)
set (HDF5_ENABLE_Z_LIB_SUPPORT OFF)
set (HDF5_ENABLE_SZIP_SUPPORT  OFF)
set (HDF5_ENABLE_SZIP_ENCODING )
set (HDF5_BUILD_SHARED_LIBS    OFF)

#-----------------------------------------------------------------------------
# Dependencies
#-----------------------------------------------------------------------------
IF(HDF5_ENABLE_PARALLEL)
  SET(HDF5_MPI_C_INCLUDE_PATH "")
  SET(HDF5_MPI_C_LIBRARIES    "")
ENDIF(HDF5_ENABLE_PARALLEL)

#-----------------------------------------------------------------------------
# Directories
#-----------------------------------------------------------------------------
set (HDF5_INCLUDE_DIR "/home/sz/deep/rnnlib_openblas/hdf5/src;/home/sz/deep/rnnlib_openblas/hdf5/c++;/home/sz/deep/rnnlib_openblas/hdf5/hl;/home/sz/deep/rnnlib_openblas/hdf5/tools;/home/sz/deep/rnnlib_openblas/hdf5" "${HDF5_MPI_C_INCLUDE_PATH}" )

if (HDF5_BUILD_FORTRAN)
  set (HDF5_INCLUDE_DIR_FORTRAN "/home/sz/deep/rnnlib_openblas/hdf5/bin" )
endif (HDF5_BUILD_FORTRAN)
  
if (HDF5_BUILD_CPP_LIB)
  set (HDF5_INCLUDE_DIR_CPP ${HDF5_INCLUDE_DIR} )
endif (HDF5_BUILD_CPP_LIB)

if (HDF5_BUILD_HL_LIB)
  set (HDF5_INCLUDE_DIR_HL ${HDF5_INCLUDE_DIR} )
endif (HDF5_BUILD_HL_LIB)

if (HDF5_BUILD_HL_LIB AND HDF5_BUILD_CPP_LIB)
  set (HDF5_INCLUDE_DIR_HL_CPP ${HDF5_INCLUDE_DIR} )
endif (HDF5_BUILD_HL_LIB AND HDF5_BUILD_CPP_LIB)

if (HDF5_BUILD_TOOLS)
  set (HDF5_INCLUDE_DIR_TOOLS ${HDF5_INCLUDE_DIR} )
endif (HDF5_BUILD_TOOLS)

if (HDF5_BUILD_SHARED_LIBS)
  set (H5_BUILT_AS_DYNAMIC_LIB 1 )
else (HDF5_BUILD_SHARED_LIBS)
  set (H5_BUILT_AS_STATIC_LIB 1 )
endif (HDF5_BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Version Strings
#-----------------------------------------------------------------------------
set (HDF5_VERSION_STRING 1.9.178)
set (HDF5_VERSION_MAJOR  1.9)
set (HDF5_VERSION_MINOR  178)

#-----------------------------------------------------------------------------
# Don't include targets if this file is being picked up by another
# project which has already build hdf5 as a subproject
#-----------------------------------------------------------------------------
if (NOT TARGET "hdf5")
  include (${SELF_DIR}/hdf5-targets.cmake)
  set (HDF5_LIBRARIES "hdf5;hdf5_hl")
endif (NOT TARGET "hdf5")
