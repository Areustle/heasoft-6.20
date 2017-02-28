HD_COMPONENT_NAME	= heagen

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= xronos

HD_LIBRARY_SRC_c	= par/xrgetparams.c \
			  par/xrgetnser.c \
			  par/xrgetfiles.c \
			  par/xrparseopt.c \
			  par/xrskimfiles.c \
			  fits/xrftgext.c \
			  fits/xrftgcol.c \
			  fits/xrftgtky.c \
			  fits/xrftgtunit.c \
			  fits/xrftgtgti.c \
			  fits/xrftgtcor.c \
			  fits/xrftgdky.c \
			  fits/xrftgdes.c \
			  par/xrsummfile.c \
			  par/xrupdtser.c \
			  par/xrgetwin.c \
			  par/xrgetbinmode.c \
                          par/xrparsebinmode.c \
			  par/xrgettime.c \
			  par/xrnewtimebreak.c \
			  par/xrgettimebreak.c \
			  par/xrgetnbin.c \
                          par/xrgindefdbl.c \
                          par/xrgindefint.c \
                          par/xrgetfout.c \
                          fits/xrftbufinit.c \
                          fits/xrftbufgetpnt.c \
			  fits/xrftdatapnt.c \
			  fits/xrftexpopnt.c \
                          fits/xrftbufclean.c \
			  interval/xrsetupfrm.c \
			  interval/xrgetintv.c \
			  interval/xrbininit.c \
			  interval/xrbinseries.c \
			  interval/xrbinpnt.c \
			  interval/xrmergebin.c \
			  interval/xraddnewbin.c \
			  interval/xrallocser.c \
			  interval/xrfreeser.c \
			  interval/xrupdtexpo.c \
			  interval/xrbayesblock.c \
                          output/xrwroutf.c \
			  util/stralloc.c \
			  util/strcatalloc.c \
			  util/xrdectun.c \
			  util/xrcnvtun.c \
			  util/xrdaytostr.c \
			  util/xrinit.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_SHLIB_LIBS		= ${HD_STD_CLIBS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= xronos.h 

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o}

include ${HD_STD_MAKEFILE}

${HD_CTASK_SRC_c:.c=.o}: xronos.h
