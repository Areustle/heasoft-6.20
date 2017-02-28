$! Command file to make and install the FIXINC program used
$! by XFOR under VMS
$!
$ FORTRAN FIXINC
$ LINK FIXINC
$ RENAME FIXINC.EXE XANBIN:[BIN]
$ FIXINC :== $XANBIN:[BIN]FIXINC.EXE
$ DEL/NOCONF FIXINC.OBJ;*

