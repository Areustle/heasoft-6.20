HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= brcaldb

HD_CTASK_SRC_c		= hbrcaldb.c brcaldb.c bryfuncs.c copy_cmd.c \
			  lex_cb.c show_cmd.c y_tab.c

HD_CFLAGS		= ${HD_STD_CFLAGS} -I./include

HD_CLIBS		= ${HD_STD_CLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= brcaldb.par

HD_INSTALL_HELP		= brcaldb.hlp brcaldb.txt

HD_INSTALL_EXTRA	= install-brcaldbfiles

BRCALDB_FILES		= brcaldb.cmd brcaldb.cmd1 brcaldb.key brcaldb.kw \
			  brcaldb.shf brcaldb.udc1

include ${HD_STD_MAKEFILE}

install-brcaldbfiles:
	@for file in ${BRCALDB_FILES}; do \
		${HD_INSTALL} $$file ${HD_BIN}/$$file ${HD_CP_P}; \
	done
