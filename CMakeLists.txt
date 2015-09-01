cmake_minimum_required (VERSION 2.6)
project (rnnlib)

execute_process ( COMMAND git submodule update --init boost )

SET ( PREFER_MKL "FALSE" CACHE BOOLEAN "Whether to link against Intel's MKL or OpenBlas" )

SET ( MY_OWN_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/install" CACHE PATH "Prefix prepended to
install directories" )
SET ( CMAKE_INSTALL_PREFIX "${MY_OWN_INSTALL_PREFIX}" CACHE INTERNAL "Prefix
prepended to install directories" FORCE )
set( CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} ${CMAKE_SOURCE_DIR}/install/lib )

FIND_LIBRARY(openblas_LIB NAMES libopenblas.a openblas PATHS install/lib NO_DEFAULT_PATH)
IF(NOT openblas_LIB)
       execute_process ( COMMAND git submodule update --init openblas )
       execute_process ( COMMAND make  NO_LAPACKE=1 NO_SHARED=1
                       WORKING_DIRECTORY openblas )
       execute_process ( COMMAND make install NO_SHARED=1 NO_LAPACKE=1 PREFIX=${CMAKE_SOURCE_DIR}/install
                       WORKING_DIRECTORY openblas )
       FIND_LIBRARY(openblas_LIB NAMES libopenblas.a openblas PATHS install/lib NO_DEFAULT_PATH)
ENDIF()

FIND_LIBRARY(hdf5_LIB NAMES libhdf5.a hdf5 PATH install/lib NO_DEFAULT_PATH )
FIND_LIBRARY(hdf5hl_LIB NAMES libhdf5_hl.a hdf5_hl PATHS install/lib NO_DEFAULT_PATH )
IF(NOT hdf5_LIB)
       execute_process ( COMMAND cmake -DHDF5_BUILD_HL_LIB=ON -DCMAKE_INSTALL_PREFIX='${CMAKE_SOURCE_DIR}/install' . 
                       WORKING_DIRECTORY hdf5_snap )
       execute_process ( COMMAND [ -f src/H5pubconf.h ] || cp H5pubconf.h src/
                       WORKING_DIRECTORY hdf5_snap )
       execute_process ( COMMAND cmake --build .
                       WORKING_DIRECTORY hdf5_snap )
       execute_process ( COMMAND make install
                       WORKING_DIRECTORY hdf5_snap )
       FIND_LIBRARY(hdf5_LIB NAMES libhdf5.a hdf5 PATHS install/lib NO_DEFAULT_PATH )
       FIND_LIBRARY(hdf5hl_LIB NAMES libhdf5_hl.a hdf5_hl PATHS install/lib NO_DEFAULT_PATH )
ENDIF()



FIND_LIBRARY ( netcdf_LIB NAMES libnetcdf.a netcdf PATHS 
		install/lib
		install/lib/x86_64-linux-gnu
             NO_DEFAULT_PATH )
IF(NOT netcdf_LIB)
       execute_process ( COMMAND git submodule update --init netcdf-c )

       execute_process ( COMMAND cmake -DBUILD_UTILITIES=OFF -DENABLE_TESTS=OFF -DCMAKE_INSTALL_PREFIX='${CMAKE_SOURCE_DIR}/install' -DENABLE_DAP=ON -DBUILD_SHARED_LIBS=OFF   .
                       WORKING_DIRECTORY netcdf-c )
       execute_process ( COMMAND cmake --build -DBUILD_SHARED_LIBS=OFF .
                       WORKING_DIRECTORY netcdf-c )
       execute_process ( COMMAND make install 
                       WORKING_DIRECTORY netcdf-c )
       FIND_LIBRARY ( netcdf_LIB NAMES libnetcdf.a netcdf 
		      PATHS install/lib
		            install/lib/x86_64-linux-gnu
	              NO_DEFAULT_PATH )
ENDIF()


FIND_LIBRARY ( netcdfcxx_LIB NAMES libnetcdf_c++.a netcdf_c++ 
               PATHS install/lib
                     install/lib/x86_64-linux-gnu
               NO_DEFAULT_PATH )
IF(NOT netcdfcxx_LIB)
       execute_process ( COMMAND [ -f ./configure ] ||  autoreconf -if
                       WORKING_DIRECTORY netcdf-cxx )
       set ( ENV{CC} ${CMAKE_C_COMPILER} )
       set ( ENV{CXX} ${CMAKE_CXX_COMPILER} )
       set ( ENV{CXXFLAGS} "-I${CMAKE_SOURCE_DIR}/install/include -std=c++11" )
       IF ( APPLE )
           set ( ENV{CXXFLAGS} "-I${CMAKE_SOURCE_DIR}/install/include -std=c++11 -stdlib=libc++" )
       ENDIF()
       set ( ENV{CFLAGS} "-I${CMAKE_SOURCE_DIR}/install/include" )
       execute_process ( COMMAND ./configure   --disable-shared --prefix=${CMAKE_SOURCE_DIR}/install
                       WORKING_DIRECTORY netcdf-cxx )

       execute_process ( COMMAND make
                       WORKING_DIRECTORY netcdf-cxx )
       execute_process ( COMMAND make install
                       WORKING_DIRECTORY netcdf-cxx )

       FIND_LIBRARY ( netcdfcxx_LIB NAMES libnetcdf_c++.a netcdf_c++ 
                      PATHS install/lib
                            install/lib/x86_64-linux-gnu
                      NO_DEFAULT_PATH )
ENDIF()


IF ( APPLE ) 
       # boost warnings
       set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DBOOST_NO_CXX11_VARIADIC_TEMPLATES=1 -std=c++11 -stdlib=libc++")
       set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-register -Wno-redeclared-class-member")
       set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -std=c++11 -stdlib=libc++")
ELSE()
       set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DBOOST_NO_CXX11_VARIADIC_TEMPLATES=1 -std=c++11")
ENDIF()


file(GLOB rnnlib_SRC
    "src/*.hpp"
    "src/BivariateNorm.cpp"
    "src/DataExporter.cpp"
    "src/Log.cpp"
    "src/Main.cpp"
    "src/Random.cpp"
)
file(GLOB rnnsynth_SRC
    "src/*.hpp"
    "src/BivariateNorm.cpp"
    "src/DataExporter.cpp"
    "src/Log.cpp"
    "src/Random.cpp"
    "src/Synth.cpp"
)
file(GLOB rnnagg_SRC
    "src/*.hpp"
    "src/BivariateNorm.cpp"
    "src/DataExporter.cpp"
    "src/Log.cpp"
    "src/Random.cpp"
    "src/Agg.cpp"
)
file(GLOB rnnval_SRC
    "src/*.hpp"
    "src/BivariateNorm.cpp"
    "src/DataExporter.cpp"
    "src/Log.cpp"
    "src/Random.cpp"
    "src/Val.cpp"
)

set ( rnnlib_SRC ${rnnlib_SRC}  )
set ( rnnsynth_SRC ${rnnsynth_SRC} )
set ( rnnagg_SRC ${rnnagg_SRC} )
set ( rnnval_SRC ${rnnval_SRC} )

include_directories(src boost openblas install/include)
add_executable ( gradient_check ${rnnlib_SRC} )


MESSAGE(STATUS "Using HDF5_HL Library: ${hdf5_LIB}")
MESSAGE(STATUS "Using HDF5 Library: ${hdf5hl_LIB}")
MESSAGE(STATUS "Using NETCDF Library: ${netcdf_LIB}")
MESSAGE(STATUS "Using NETCDFCXX Library: ${netcdfcxx_LIB}")

FIND_LIBRARY ( mkl_lp64_LIB NAMES libmkl_intel_lp64.a
                      PATHS /opt/intel/mkl/lib PATH_SUFFIXES intel64)
FIND_LIBRARY ( mkl_core_LIB NAMES libmkl_core.a
                      PATHS /opt/intel/mkl/lib PATH_SUFFIXES intel64)
FIND_LIBRARY ( mkl_thread_LIB NAMES libmkl_intel_thread.a
                      PATHS /opt/intel/mkl/lib PATH_SUFFIXES intel64)
FIND_LIBRARY ( mkl_omp_LIB NAMES libiomp5.a
                      PATHS /opt/intel/lib PATH_SUFFIXES intel64)
IF ( mkl_lp64_LIB AND PREFER_MKL )
   IF ( APPLE ) 
      set ( openblas_LIB ${mkl_lp64_LIB} ${mkl_core_LIB} ${mkl_thread_LIB} ${mkl_omp_LIB} )   
   ELSE()
      set ( openblas_LIB -Wl,--start-group ${mkl_lp64_LIB} ${mkl_core_LIB} ${mkl_thread_LIB} ${mkl_omp_LIB}  -Wl,--end-group )
   ENDIF()
   MESSAGE(STATUS "Prefer MKL Libs: ${openblas_LIB}")
ELSE()
   MESSAGE(STATUS "NO MKL Libs using: ${openblas_LIB}")
ENDIF()
set ( rnnlib_LIBS  ${openblas_LIB} ${netcdfcxx_LIB} ${netcdf_LIB} ${hdf5hl_LIB} ${hdf5_LIB} curl )
IF ( UNIX ) 
	LIST ( APPEND rnnlib_LIBS pthread dl ) 
ENDIF()
target_link_libraries ( gradient_check ${rnnlib_LIBS} )

add_executable ( rnnlib ${rnnlib_SRC} )
get_target_property( TEMP rnnlib_float COMPILE_FLAGS )
IF(NOT TEMP)
       set (TEMP "")
ENDIF()
SET(TEMP "${TEMP} -DFLOAT_REALS" )
set_target_properties( rnnlib PROPERTIES COMPILE_FLAGS ${TEMP} )
target_link_libraries ( rnnlib ${rnnlib_LIBS} )

add_executable ( rnnsynth ${rnnsynth_SRC} )
set_target_properties( rnnsynth PROPERTIES COMPILE_FLAGS ${TEMP} )
target_link_libraries ( rnnsynth ${rnnlib_LIBS} )

add_executable ( rnnagg ${rnnagg_SRC} )
set_target_properties( rnnagg PROPERTIES COMPILE_FLAGS ${TEMP} )
target_link_libraries ( rnnagg ${rnnlib_LIBS} )

add_executable ( rnnval ${rnnval_SRC} )
set_target_properties( rnnval PROPERTIES COMPILE_FLAGS ${TEMP} )
target_link_libraries ( rnnval ${rnnlib_LIBS} )
