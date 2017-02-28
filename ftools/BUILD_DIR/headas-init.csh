# Filename: headas-init.csh
# Description: C-shell flavor initialization for all HEADAS software
#              runs headas-setup to generate a temporary csh script
#              tailored specifically to this user and HEADAS software
#              installation, then source that.
# Author/Date: James Peachey, HEASARC/GSFC/NASA, May 3, 1999
# Modified for HEADAS December 2001
#
if(${?HEADAS} == 0) then 
  echo "headas-init.csh: ERROR -- set HEADAS before sourcing headas-init.csh"
else if(-x "$HEADAS/BUILD_DIR/headas-setup") then 
  set headas_init=`$HEADAS/BUILD_DIR/headas-setup csh`
  if($status == 0 && "x$headas_init" != x) then
    if(-f "$headas_init") then
      source $headas_init
    endif
    \rm -f $headas_init
  endif
  unset headas_init
else
  echo "headas-init.csh: ERROR -- cannot execute $HEADAS/BUILD_DIR/headas-setup"
endif
