# Generated automatically from Makefile.in by configure.
# Makefile.in generated automatically by automake 1.5 from Makefile.am.

# Copyright 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
# Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.



SHELL = /bin/sh

srcdir = .
top_srcdir = ..
prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/info
mandir = ${prefix}/man
includedir = ${prefix}/include
oldincludedir = /usr/include
pkgdatadir = $(datadir)/sextractor
pkglibdir = $(libdir)/sextractor
pkgincludedir = $(includedir)/sextractor
top_builddir = ..

ACLOCAL = ${SHELL} /Home/lhea1/wiegand/work/sextractor-2.3.1_lanl/missing --run aclocal
AUTOCONF = ${SHELL} /Home/lhea1/wiegand/work/sextractor-2.3.1_lanl/missing --run autoconf
AUTOMAKE = ${SHELL} /Home/lhea1/wiegand/work/sextractor-2.3.1_lanl/missing --run automake
AUTOHEADER = ${SHELL} /Home/lhea1/wiegand/work/sextractor-2.3.1_lanl/missing --run autoheader

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL_PROGRAM}
INSTALL_HEADER = $(INSTALL_DATA)
transform = s,x,x,
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
host_alias = i686-pc-linux-gnu
host_triplet = i686-pc-linux-gnu
AMTAR = ${SHELL} /Home/lhea1/wiegand/work/sextractor-2.3.1_lanl/missing --run tar
AS = @AS@
AWK = gawk
CC = gcc
CPP = gcc -E
DEPDIR = .deps
DLLTOOL = @DLLTOOL@
DOCBOOK2HTML = 'db2html'
ECHO = echo
EXEEXT = 
GLIB_CFLAGS = -I/usr/include/glib-1.2 -I/usr/lib/glib/include
GLIB_CONFIG = /usr/bin/glib-config
GLIB_LIBS = -L/usr/lib -lglib
GNOME_DOC = $SRCDIR/gnome-doc
HAVE_GNOME_DOC = 1
INSTALL_STRIP_PROGRAM = ${SHELL} $(install_sh) -c -s
LIBTOOL = $(SHELL) $(top_builddir)/libtool
LN_S = ln -s
OBJDUMP = @OBJDUMP@
OBJEXT = o
PACKAGE = sextractor
RANLIB = ranlib
STRIP = strip
VERSION = 2.3.1_lanl
am__include = include
am__quote = 
install_sh = /Home/lhea1/wiegand/work/sextractor-2.3.1_lanl/install-sh

pkgdata_DATA = block_3x3.conv default.conv default.nnw default.param \
	default.sex gauss_1.5_3x3.conv gauss_2.0_3x3.conv gauss_2.0_5x5.conv \
	gauss_2.5_5x5.conv gauss_3.0_5x5.conv gauss_3.0_7x7.conv \
	gauss_4.0_7x7.conv gauss_5.0_9x9.conv mexhat_1.5_5x5.conv \
	mexhat_2.0_7x7.conv mexhat_2.5_7x7.conv mexhat_3.0_9x9.conv \
	mexhat_4.0_9x9.conv mexhat_5.0_11x11.conv tophat_1.5_3x3.conv \
	tophat_2.0_3x3.conv tophat_2.5_3x3.conv tophat_3.0_3x3.conv \
	tophat_4.0_5x5.conv tophat_5.0_5x5.conv


EXTRA_DIST = README $(pkgdata_DATA)
subdir = config
mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_CLEAN_FILES =
DIST_SOURCES =
DATA = $(pkgdata_DATA)

DIST_COMMON = README Makefile.am Makefile.in
all: all-am

.SUFFIXES:

mostlyclean-libtool:
	-rm -f *.lo

clean-libtool:
	-rm -rf .libs _libs

distclean-libtool:
	-rm -f libtool
$(srcdir)/Makefile.in:  Makefile.am  $(top_srcdir)/configure.in $(ACLOCAL_M4)
	cd $(top_srcdir) && \
	  $(AUTOMAKE) --gnu  config/Makefile
Makefile:  $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	  CONFIG_HEADERS= CONFIG_LINKS= \
	  CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
uninstall-info-am:
install-pkgdataDATA: $(pkgdata_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)
	@list='$(pkgdata_DATA)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " $(INSTALL_DATA) $$d$$p $(DESTDIR)$(pkgdatadir)/$$f"; \
	  $(INSTALL_DATA) $$d$$p $(DESTDIR)$(pkgdatadir)/$$f; \
	done

uninstall-pkgdataDATA:
	@$(NORMAL_UNINSTALL)
	@list='$(pkgdata_DATA)'; for p in $$list; do \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " rm -f $(DESTDIR)$(pkgdatadir)/$$f"; \
	  rm -f $(DESTDIR)$(pkgdatadir)/$$f; \
	done
tags: TAGS
TAGS:


DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)

top_distdir = ..
distdir = $(top_distdir)/$(PACKAGE)-$(VERSION)

distdir: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  if test -f $$file; then d=.; else d=$(srcdir); fi; \
	  dir=`echo "$$file" | sed -e 's,/[^/]*$$,,'`; \
	  if test "$$dir" != "$$file" && test "$$dir" != "."; then \
	    $(mkinstalldirs) "$(distdir)/$$dir"; \
	  fi; \
	  if test -d $$d/$$file; then \
	    cp -pR $$d/$$file $(distdir) \
	    || exit 1; \
	  else \
	    test -f $(distdir)/$$file \
	    || cp -p $$d/$$file $(distdir)/$$file \
	    || exit 1; \
	  fi; \
	done
check-am: all-am
check: check-am
all-am: Makefile $(DATA)

installdirs:
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)

install: install-am
install-exec: install-exec-am
install-data: install-data-am
uninstall: uninstall-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am

installcheck: installcheck-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	  `test -z '$(STRIP)' || \
	    echo "INSTALL_PROGRAM_ENV=STRIPPROG='$(STRIP)'"` install
mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES) stamp-h stamp-h[0-9]*

maintainer-clean-generic:
	@echo "This command is intended for maintainers to use"
	@echo "it deletes files that may require special tools to rebuild."
clean: clean-am

clean-am: clean-generic clean-libtool mostlyclean-am

distclean: distclean-am

distclean-am: clean-am distclean-generic distclean-libtool

dvi: dvi-am

dvi-am:

info: info-am

info-am:

install-data-am: install-pkgdataDATA

install-exec-am:

install-info: install-info-am

install-man:

installcheck-am:

maintainer-clean: maintainer-clean-am

maintainer-clean-am: distclean-am maintainer-clean-generic

mostlyclean: mostlyclean-am

mostlyclean-am: mostlyclean-generic mostlyclean-libtool

uninstall-am: uninstall-info-am uninstall-pkgdataDATA

.PHONY: all all-am check check-am clean clean-generic clean-libtool \
	distclean distclean-generic distclean-libtool distdir dvi \
	dvi-am info info-am install install-am install-data \
	install-data-am install-exec install-exec-am install-info \
	install-info-am install-man install-pkgdataDATA install-strip \
	installcheck installcheck-am installdirs maintainer-clean \
	maintainer-clean-generic mostlyclean mostlyclean-generic \
	mostlyclean-libtool uninstall uninstall-am uninstall-info-am \
	uninstall-pkgdataDATA

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
