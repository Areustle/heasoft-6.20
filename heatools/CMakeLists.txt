PROJECT(heatools)
SUBDIRS(ftappend ftcalc ftchecksum ftcopy ftcreate ftdelhdu ftdelrow ftdiff ftedit ftemplate fthedit ftimgcreate ftinsert ftkeypar ftlist ftmerge ftpaste ftselect ftsort ftstat ftverify fhelp)
LINK_LIBRARIES(hdinit hdutils ape cfitsio hdio)
