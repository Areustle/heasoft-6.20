# Example makefile to print the documentation of QDP
# consult your local TeX/LaTeX guru if you need help.

# NOTE: it is NORMAL to get on first pass a lot of :
#	LaTeX Warning: Reference `xxx' on page xx undefined.

all:	split

# This is an EXAMPLE of how to print a .dvi file. Refer to your local TeX
# Wizard for more details
# you will need a dvi2ps understanding the \special{psfile=...} command
# best one is dvips v5.0, anonymous ftp on labrea.stanford.edu (36.8.0.47)
# DO NOT FORGET TO INCLUDE THE idraw.pro POSTSCRIPT HEADER!!!
# (strip_ps is a local filter to strip ps comments, you can omit it,
# or use "squeeze" coming with the dvips distribution)

split: qdp.dvi idraw.pro
	rm -rf split; mkdir split
	for i in 001 026 051 076 101 ;do \
dvips -p $$i -n 25 -h idraw.pro -o split/pages-$$i.ps qdp.dvi ;\
done

clean:
	rm -rf split *.aux *.dvi *.toc *.log

.SUFFIXES: .tex .dvi
.tex.dvi:
	latex $*
