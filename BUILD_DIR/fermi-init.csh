# Filename: fermi-init.csh
# Description: C-shell flavor initialization for all FERMI software
#              runs fermi-setup to generate a temporary csh script
#              tailored specifically to this user and FERMI software
#              installation, then source that.
# Author/Date: James Peachey, HEASARC/GSFC/NASA, May 3, 1999
# Modified for HEADAS December 2001
# Adapted for FERMI September 2008
#
if(${?FERMI_DIR} == 0) then 
  echo "fermi-init.csh: ERROR -- set FERMI_DIR before sourcing fermi-init.csh"
else if(-x "$FERMI_DIR/BUILD_DIR/fermi-setup") then 
  set FERMI_INST_DIR=$FERMI_DIR
  set fermi_init=`$FERMI_INST_DIR/BUILD_DIR/fermi-setup csh`
  if($status == 0 && "x$fermi_init" != x) then
    if(-f "$fermi_init") then
      source $fermi_init
    endif
    \rm -f $fermi_init
  endif
  unset fermi_init
else
  echo "fermi-init.csh: ERROR -- cannot execute $FERMI_DIR/BUILD_DIR/fermi-setup"
endif
