# This is a makefile for ASCA_ANL manual
#	by Y.Ishisaki	31-Jul-1994

SOURCE = manual.tex mailing_list.tex asca_anl_contrib.tex \
         introduction.tex getting_started.tex on_your_own.tex \
         reference_for_expert.tex utility_routines.tex builtin_modules.tex \
         details_in_Fortran.tex details_in_C.tex \
         installation.tex \
         bnk.tex evs.tex hbook.tex cli_com.tex bibliography.tex

FIGURE = bnkkey-v0.80.eps call_module-v0.80-flip.eps

.dvi.ps:
	dvi2ps $< > $@

.SUFFIXES:

.SUFFIXES: .ps .dvi .toc .tex

manual.ps: manual.dvi

manual.dvi: ${SOURCE} ${FIGURE}
	latex $*; latex $*; latex $*; 

clean:
	${RM} manual.aux manual.lof manual.log manual.toc
