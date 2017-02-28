#! /bin/csh

set thisdir=`pwd`

if(${?HEADAS} == 0) then
  echo "heasim-init.csh: ERROR -- set HEADAS before sourcing heasim-init.csh"
else
    cd $HEADAS/../heasim/BUILD_DIR
    set heasim_BUILD_DIR=`pwd`
    set build_dir_found=`echo $PATH | grep $heasim_BUILD_DIR`

    if( "x$build_dir_found" == "x") then
	setenv PATH ${PATH}:$heasim_BUILD_DIR
    else
	echo "heasim BUILD_DIR already found in PATH."
    endif


    cd $thisdir
endif

