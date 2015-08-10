#                                               -*- Autoconf -*-

# This is part of Unidata's netCDF legacy C++ library. Copyright
# 2005-2011, see the COPYRIGHT file for more information.

# Running autoconf on this file will trigger a warning if 
# autoconf is not at least the specified version.
AC_PREREQ([2.59])

# Initialize with name, version, and support email address. 
AC_INIT([netCDF-cxx], [4.2], [support-netcdf@unidata.ucar.edu])

# Create the VERSION file, which contains the package version from
# AC_INIT.
echo -n AC_PACKAGE_VERSION>VERSION
AC_SUBST(PACKAGE_VERSION)

AC_MSG_NOTICE([netCDF-cxx AC_PACKAGE_VERSION])

# Keep libtool macros in an m4 directory.
AC_CONFIG_MACRO_DIR([m4])

# Find out about the host we're building on.
AC_CANONICAL_HOST

# Find out about the target we're building for.
AC_CANONICAL_TARGET

# We will output a config.h.
AC_CONFIG_HEADERS([config.h])

# This call is required by automake.
AM_INIT_AUTOMAKE([foreign dist-bzip2 dist-zip dist-tarZ subdir-objects])

# Check for the existance of this file before proceeding.
AC_CONFIG_SRCDIR([cxx/ncvalues.h])

AC_MSG_NOTICE([checking user options])

# Does the user want to run extra tests with valgrind?
AC_MSG_CHECKING([whether extra valgrind tests should be run])	
AC_ARG_ENABLE([valgrind-tests],
              [AS_HELP_STRING([--enable-valgrind-tests],
                              [build with valgrind-tests (valgrind is required, static builds only)])])
test "x$enable_valgrind_tests" = xyes || enable_valgrind_tests=no
AC_MSG_RESULT($enable_valgrind_tests)

# Does the user want to do some extra tests?
AC_MSG_CHECKING([whether netCDF extra tests should be run (developers only)])
AC_ARG_ENABLE([extra-tests],
              [AS_HELP_STRING([--enable-extra-tests],
                              [run some extra tests that may not pass because of known issues])])
test "x$enable_extra_tests" = xyes || enable_extra_tests=no
AC_MSG_RESULT($enable_extra_tests)
if test "x$enable_extra_tests" = xyes; then
   AC_DEFINE([EXTRA_TESTS], [1], [if true, run extra tests which may not work yet])
fi
AM_CONDITIONAL(EXTRA_TESTS, [test x$enable_extra_tests = xyes])

# Does the user want to run tests for large files (> 2GiB)?
AC_MSG_CHECKING([whether large file (> 2GB) tests should be run])
AC_ARG_ENABLE([large-file-tests],
              [AS_HELP_STRING([--enable-large-file-tests],
                              [Run tests which create very large data files (~13 GB disk space
                              required, but it will be recovered when tests are complete). See
                              option --with-temp-large to specify temporary directory])])
test "x$enable_large_file_tests" = xyes || enable_large_file_tests=no
AC_MSG_RESULT($enable_large_file_tests)
AM_CONDITIONAL(LARGE_FILE_TESTS, [test x$enable_large_file_tests = xyes])
if test "x$enable_large_file_tests" = xyes; then
   AC_DEFINE([LARGE_FILE_TESTS], [1], [do large file tests])
fi

# If the env. variable TEMP_LARGE is set, or if
# --with-temp-large=<directory>, use it as a place for the large
# (i.e. > 2 GiB) files created during the large file testing.
AC_MSG_CHECKING([where to put large temp files if large file tests are run])
AC_ARG_WITH([temp-large],
            [AS_HELP_STRING([--with-temp-large=<directory>],
                            [specify directory where large files (i.e. >2 GB) \
                            will be written, if large files tests are run with
                            --enable-large-file-tests])],
            [TEMP_LARGE=$with_temp_large])
TEMP_LARGE=${TEMP_LARGE-.}
AC_MSG_RESULT($TEMP_LARGE)
#AC_SUBST(TEMP_LARGE)
AC_DEFINE_UNQUOTED([TEMP_LARGE], ["$TEMP_LARGE"], [Place to put very large netCDF test files.])

AC_MSG_NOTICE([finding C++ compiler (will not be used if C++ API is not desired)])
AC_PROG_CXX

# Set up libtool.
AC_MSG_NOTICE([setting up libtool])
LT_PREREQ([2.2])
LT_INIT

# Valgrind tests don't work with shared builds because of some libtool
# weirdness.
if test "x$enable_shared" = xyes; then
   if test "x$enable_valgrind_tests" = xyes; then
      AC_MSG_ERROR([No valgrind tests with shared libraries])
   fi
fi

AC_MSG_NOTICE([finding other utilities])

# Find the install program.
AC_PROG_INSTALL

# Check to see if any macros must be set to enable large (>2GB) files.
AC_SYS_LARGEFILE

# Find the netCDF header and library.
AC_CHECK_HEADERS([netcdf.h], [], [AC_MSG_ERROR([netcdf.h could not be found. Please set CPPFLAGS.])])
AC_SEARCH_LIBS([nc_open], [netcdf])

# See if these functions are in the library.
AC_CHECK_FUNCS([nc_def_opaque nccreate nc_set_log_level nc_use_parallel_enabled])

# Automake conditionals need to be called, whether the answer is yes
# or no.
AM_CONDITIONAL(USE_VALGRIND_TESTS, [test "x$enable_valgrind_tests" = xyes])
AM_CONDITIONAL([USE_NETCDF4], [test -n "$HAVE_NC_DEF_OPAQUE"])

AC_MSG_NOTICE([generating header files and makefiles])
AC_CONFIG_FILES([Makefile
                 man4/Makefile
                 cxx/Makefile
                 examples/Makefile])

AC_OUTPUT()

