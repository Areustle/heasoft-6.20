HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= a2source

HD_CTASK_SRC_c		= ha2source.c

HD_CTASK_SRC_f		= a2source.f create_filelist.f daytofile.f degoff.f \
			  ephem2.f getpars.f process_coords.f write_filelist.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= a2source.parin

HD_INSTALL_HELP		= a2source.hlp a2source.txt

include ${HD_STD_MAKEFILE}
