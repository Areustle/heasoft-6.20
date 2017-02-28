#  Filename: headas-init.sh
# Description: Bourne-shell flavor initialization for all HEADAS software.
#              Runs headas-setup to generate a sh script tailored
#              specifically to this user and LHEA software
#              installation, then source that.
# Author/Date: James Peachey, HEASARC/GSFC/NASA, May 3, 1999
#
if [ "x$HEADAS" = x ]; then 
  echo "headas-init.sh: ERROR -- set HEADAS before sourcing headas-init.sh"
elif [ -x "$HEADAS/BUILD_DIR/headas-setup" ]; then 
  headas_init=`$HEADAS/BUILD_DIR/headas-setup sh`
  if [ $? -eq 0 -a "x$headas_init" != x ]; then
    if [ -f "$headas_init" ]; then
      . $headas_init
    fi
    rm -f $headas_init
  fi
  unset headas_init
else
  echo "headas-init.sh: ERROR -- cannot execute $HEADAS/BUILD_DIR/headas-setup"
fi
