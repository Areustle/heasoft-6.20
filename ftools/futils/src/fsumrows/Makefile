HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fsumrows

HD_CTASK_SRC_c		= hfsumrows.c fsumrows_build_selected_columns_list.c \
			  fsumrows.c fsumrows_column_spec_to_number.c \
			  fsumrows_get_params.c fsumrows_process_rows.c \
			  fsumrows_wrt_col_result.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_STD_CLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fsumrows.par

HD_INSTALL_HELP		= fsumrows.hlp fsumrows.txt

include ${HD_STD_MAKEFILE}
