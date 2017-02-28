#!/bin/sh
#
# standalone.sh: script to build the standalone version of ascatape for
# unix systems. By Don Jennings, 05/1994
#
# before the standalone version of ascatape can be compiled, one line of
# code within the ascatape.f file must be changed. Edit this file and
# replace the statement:
#   
#                standalone = .false.
#
# with
#
#                standalone = .true.
#
# define the following variable to point to the location of your local
# FITSIO library
#
fitsio='/usr/local/lib/libfitsio.a'
#
# for the following compile statements, make sure the f77 and cc compiler
# executables are in your path. Solaris users must use the following 
# statement to compile the tapeio.c module
#
# cc -c -Dsolaris -o tapeio.o tapeio.c
#
cc -c -o tapeio.o tapeio.c
f77 -c -o utility.o utility_unix.f
f77 -c -o subdir.o subdir_sort.f
f77 -c -o ascate.o ascatape.f
f77 standalone.f -o ascatape ascate.o utility.o subdir.o tapeio.o $fitsio
exit
