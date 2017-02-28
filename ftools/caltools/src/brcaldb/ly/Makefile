#Makefile for brcaldb lexer and parser
#     Ron Zellar
#     NASA GSFC
#     Jan 27, 1995
#
#This file is site dependent

#This Makefile builds the lexical analyzer and parser code
#used by brcaldb.  It should not be necessary to build the
#lexer and parser code to compile the brcaldb program.  The
#source code for the lexer and parser was built at GSFC and
#shipped in the directory above.
#
#If you need to compile the lexer and parser, edit the 
#following lines to change the lex and yacc compilers.  
#Since command line options differ for the various lexers 
#and parsers, your site may need to change some of the build 
#lines below. This file is now configured to use flex 
#(version 2.4+) and bison.

LEX =		flex
YACC = 		bison

all:		y_tab.c lex_cb.c
		mv y_tab.c ../y_tab.c
		mv y_tab.h ../include/y_tab.h
		mv lex.cb.c ../lex_cb.c
		cp bryfuncs.c ../bryfuncs.c

y_tab.c:	brcaldb.y
		${YACC} -oy_tab.c -dy -pcb brcaldb.y

lex_cb.c:	brcaldb.l
		${LEX} -Pcb brcaldb.l
