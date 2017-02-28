#!/bin/csh
open -a X11
if (! $?DISPLAY) then
      setenv DISPLAY :0
endif
$1/Contents/Resources/fv5.4/fv $2
