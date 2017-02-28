HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_SUBDIRS		= uvot2pha uvotapercorr uvotapplywcs uvotaspcorr \
			  uvotattcorr uvotbadpix uvotcentroid uvotcoincidence \
			  uvotconvreg uvotdetect uvotevgrism uvotevtlc \
			  uvotexpcorr uvotexpmap uvotflagqual uvotflatfield \
			  uvotflux uvotgraspcorr uvotgrplot uvotgrblc \
			  uvotimage uvotimgrism uvotimsum uvotinteg uvotlss \
			  uvotmag uvotmaghist uvotmodmap uvotoptsum uvotpict \
			  uvotproduct uvotrmfgen uvotscreen uvotsequence \
			  uvotshiftpha uvotskycorr uvotskylss uvotsource \
			  uvottfc uvotunicorr

HD_TEST_SUBDIRS		= $(HD_SUBDIRS)

include ${HD_STD_MAKEFILE}

# Special dependencies for Wiki documentation
HELP_DOCS 		= $(wildcard */*.html)
WIKI_DOCS		= $(HELP_DOCS:.html=.wiki)
HTML2WIKI		= $(HOME)/bin/html2wiki

wiki-docs: $(WIKI_DOCS)

%.wiki: %.html 
	$(HTML2WIKI) $< $@
	touch $@_updated
