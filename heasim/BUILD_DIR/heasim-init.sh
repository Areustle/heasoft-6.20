#! /bin/sh

thisdir=`pwd`

if [ "x$HEADAS" = x ]; then
  echo "heasim-init.sh: ERROR -- set HEADAS before sourcing heasim-init.sh"

else
    cd $HEADAS/../heasim/BUILD_DIR
    heasim_BUILD_DIR=`pwd`
    build_dir_found=`echo $PATH | grep $heasim_BUILD_DIR`

    if [ "x$build_dir_found" == "x" ]; then
	export PATH=${PATH}:$heasim_BUILD_DIR
    else
	echo "heasim BUILD_DIR already found in PATH."
    fi


    cd $thisdir
fi



