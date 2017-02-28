c Include file for information in the data subspace keywords
c    dstyp        c*80         Values of the DSTYPi keywords
c    dsval        c*80         Values of the iDSVALj keywords
c    dsref        c*80         Values of the iDSREFj keywords
c    dsuni        c*80         Values of the DSUNIi keywords
c    dsform       c*80         Values of the DSFORM keywords
c    ndstyp       i            The number of DSTYP keywords
c    idstime      i            The i for which DSTYPi == TIME
c    idsval       i            The number of #DSVALi keywords
c    idsref       i            The number of #DSREFi keywords


      character(80)        dstyp(MAXCOLS)
      character(80)        dsuni(MAXCOLS), dsform(MAXCOLS)
      character(80)        dsval(MAXSVAL,MAXCOLS)
      character(80)        dsref(MAXSVAL,MAXCOLS)
      INTEGER             ndstyp, idstime
      INTEGER             idsval(MAXCOLS)
      INTEGER             idsref(MAXCOLS)

      COMMON / ext_sub__keys / dstyp, dsuni, dsform, dsval, dsref, 
     &                         ndstyp, idstime, idsval, idsref


