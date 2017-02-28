# 
# $Id: Makefile.am,v 1.1 2013/06/14 15:34:02 kaa Exp $
#
# Author: Paul_Kunz@slac.stanford.edu
#

# The following is set; otherwise, it would have to follow GNU
# conventions.
AUTOMAKE_OPTIONS = foreign

EXTRA_DIST =					\
	Minuit2.html Doxyfile.in

docs:
	if test -d html; then \
	(rm -rf html ) \
	fi
	doxygen Doxyfile
	mv Minuit.tag html/.

clean-local:
	if test -d html; then \
	(cd html; rm -rf *.html *.gif *.css *.dot *.png formula.repository; \
	rm -rf *.md5 *.map *.tag installdox ) \
	fi
	if test -d latex; then \
	( cd latex; rm -rf *tex *.eps *.sty ) \
	fi

