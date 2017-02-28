HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= temp2gain

HD_CTASK_SRC_c		= htemp2gain.c act_allocfile.c act_expandfn.c \
			  act_hdouble.c act_misc.c act_msg.c act_splitpath.c \
			  gisget.c temp2gain.c yfrfopen.c ylogopen.c \
			  ysfopen.c ytlmopen.c

# unused: hdouble.c expandfn.c  readallocfile.c splitpth.c 

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= temp2gain.par

HD_INSTALL_HELP		= temp2gain.txt

include ${HD_STD_MAKEFILE}
