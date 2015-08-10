## This is a automake file, part of Unidata's netCDF package.
# Copyright 2005-2006, see the COPYRIGHT file for more information.
# This file builds the netcdf documentation.

# The "all" target does nothing in this directory. The "dist" target
# *does* build the documentation, because the distribution contains
# all of the built documentation.

# The "install" target installs the documentation in a directory
# called $(prefix)/doc/$(PACKAGE)-$(VERSION).

# These are the source files for the netcdf cxx manual.
info_TEXINFOS = netcdf-cxx.texi

# Get lists of the coresponding ps, info, and pdf files.
html_docs = ${info_TEXINFOS:.texi=.html} 
html_dirs = ${info_TEXINFOS:.texi=} 
ps_docs = ${info_TEXINFOS:.texi=.ps} 
dvi_docs = ${info_TEXINFOS:.texi=.dvi} 
pdf_docs = ${info_TEXINFOS:.texi=.pdf}
info_docs= ${info_TEXINFOS:.texi=.info}
txt_docs= ${info_TEXINFOS:.texi=.txt}

# These files will be included with the dist.
EXTRA_DIST = $(pdf_docs) $(html_mans) $(txt_docs)	\
$(dvi_docs) $(ps_docs) $(info_docs) $(html_docs)

# The manual depends on defines.texi for some common definitions.
netcdf_cxx_TEXINFOS = defines.texi

# Get texi2dvi to shut the hell up, and work better for parallel
# builds. The --tidy option may be useful getting parallel builds to
# work someday.
TEXI2DVI = texi2dvi -s 

# Turn off parallel builds in this directory.
.NOTPARALLEL:

# Rule to generate the text copies of the documentation.
.texi.txt: 
	makeinfo -o $*.txt --no-headers $(srcdir)/$<

netcdf-cxx.html:
	$(MAKEINFO) --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-cxx.texi
	$(MAKEINFO) --no-split --html $(AM_MAKEINFOHTMLFLAGS) $(MAKEINFOFLAGS) -I$(srcdir) netcdf-cxx.texi

# Clean up all the junk we created!
MAINTAINERCLEANFILES = $(ps_docs) $(pdf_docs) $(txt_docs) $(info_docs)	\
$(html_docs) stamp-* version* 
