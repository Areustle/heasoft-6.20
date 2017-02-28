HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= exomerge

HD_CTASK_SRC_c		= hexomerge.c

HD_CTASK_SRC_f		= exfssco.f exmerge.f exogti.f exomerge.f \
			  exomergeinit.f exomergemake.f exowrite.f \
			  exshuffle.f exstarmode.f extimekey.f mall.f sunco.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

# Do not use -fast on Solaris (using FOPT_SYS instead of FOPT):
HD_FFLAGS		= ${FOPT_SYS} ${FFLAGS} ${HD_IFLAGS} ${DEFS}

HD_CLIBS		= ${HD_LFLAGS} -lexo${FT_VERS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= exomerge.par

HD_INSTALL_HELP		= exomerge.hlp exomerge.txt

include ${HD_STD_MAKEFILE}
