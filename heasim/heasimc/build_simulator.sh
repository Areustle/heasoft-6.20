#! /bin/sh


######## subroutine to get time ellapsed. #######################

time_ellapsed(){

    totaldate1=$1

    weekday1=`echo $totaldate1 | cut -d " " -f1`
    month1=`echo $totaldate1 | cut -d " " -f2`
    day1=`echo $totaldate1 | cut -d " " -f3`
    time1=`echo $totaldate1 | cut -d " " -f4`
    zone1=`echo $totaldate1 | cut -d " " -f5`
    year1=`echo $totaldate1 | cut -d " " -f6`
    hour1=`echo $time1 | cut -d : -f1 | sed 's/^0*//'`
    min1=`echo $time1 | cut -d : -f2 | sed 's/^0*//'`
    sec1=`echo $time1 | cut -d : -f3 | sed 's/^0*//'`
    sectime1=$((day1*24*3600 + hour1*3600 + min1*60 + sec1))

    totaldate2=$2
    weekday2=`echo $totaldate2 | cut -d " " -f1`
    month2=`echo $totaldate2 | cut -d " " -f2`
    day2=`echo $totaldate2 | cut -d " " -f3`
    time2=`echo $totaldate2 | cut -d " " -f4`
    zone2=`echo $totaldate2 | cut -d " " -f5`
    year2=`echo $totaldate2 | cut -d " " -f6`
    hour2=`echo $time2 | cut -d : -f1 | sed 's/^0*//'`
    min2=`echo $time2 | cut -d : -f2 | sed 's/^0*//'`
    sec2=`echo $time2 | cut -d : -f3 | sed 's/^0*//'`
    sectime2=$((day2*24*3600 + hour2*3600 + min2*60 + sec2))

    timediff=$((sectime2-sectime1))

    hourdiff=$(($timediff/3600))
    seconds_left=$(($timediff - $hourdiff*3600))
    if test "$hourdiff" -lt "10"; then
        hourout=`echo "0$hourdiff"`
    else
        hourout="$hourdiff"
    fi

    mindiff=$(($seconds_left/60))
    seconds_left=$(($seconds_left - $mindiff*60))
    if test "$mindiff" -lt "10"; then
        minout="0$mindiff"
    else
        minout="$mindiff"
    fi


    secdiff=$(($seconds_left))
    if test "$secdiff" -lt "10"; then
        secout="0$secdiff"
    else
        secout="$secdiff"
    fi


    echo "Elapsed: $hourout:$minout:$secout"

 }


###################### START OF THE ACTUAL SCRIPT #########################

# if we give the build script the option "nowarn" it will hide all warnings during hmake
arg1=$1

thisdir=`pwd`
builddir=$HEADAS/../heasim/BUILD_DIR
cd $builddir

# REMOVE buildtime.log if it exists
[ -f buildtime.log ] && rm buildtime.log

# insert the date and time into buildtime.log
startdate=`\date`
echo "Start: $startdate" >> buildtime.log

# start with a clean distribution
echo hmake disctlean ...
hmake distclean >& distclean.log

# make sure $HEADAS/../heasim/BUILD_DIR is in executable PATH for the duration of this script
source $HEADAS/../heasim/BUILD_DIR/heasim-init.sh


####################### EXECUTE CONFIGURE #############################################
echo ./configure ...

# These options are machine-specific!!!  
# Can copy from headas/Xspec/BUILD_DIR/config.log, if already built.
system=`uname -s`

if test "$system" = "Linux"; then
    ./configure \
	--with-heatop=$HEADAS/.. \
	--with-heacore=$HEADAS/../heacore \
	--enable-shared=yes \
	--enable-symbols=yes \
	--enable-strip=no \
	--enable-guis=no \
	--with-copt=no \
	--with-cxxopt=no \
	--with-fopt=no \
	--enable-hera=no \
	--x-libraries=NONE \
	--x-includes=NONE \
	--enable-openmp=no \
	--enable-ldopt=no \
	--enable-mac_32bit=no \
	>& config.log
elif test "$system" = "Darwin"; then
    ./configure \
	--with-heatop=$HEADAS/.. \
	--with-heacore=$HEADAS/../heacore \
	--enable-shared=yes \
	--enable-symbols=yes \
	--enable-strip=no \
	--enable-hera=no \
	--x-libraries=NONE \
	--x-includes=NONE \
	--with-copt=no \
	--with-cxxopt=no \
	--with-fopt=no \
	--enable-ldopt=no \
	--enable-mac_32bit=no \
	>& config.log
else 
    echo "System build type not identified."
    echo "configure: error" > config.log
fi

# check to see what messages are in configure
config_errors1=`grep "configure: error" config.log`
config_errors2=`grep "unrecognized option" config.log`
config_success=`grep "configure: exit 0" config.log`
if test "x$config_success" != "x"; then
    echo Configure success confirmed: $config_success
fi

##############  if we're good, EXECUTE HMAKE ###########################################
if [ "x$config_errors1$configerrors2" == "x" ] || [ "x$config_success" != "x" ]; then # if no errors
    echo "hmake ..."
    hmake >& hmake.log
else
    stopdate=`\date`
    echo "Stop:  $stopdate" >> buildtime.log
    time_ellapsed "$startdate" "$stopdate" >> buildtime.log
    echo heasim configure failed. >> buildtime.log
    more buildtime.log
    exit 1
fi

# check for error messages in hmake.log.
hmake_errors=`grep "]: \*\*\*" hmake.log`
hmake_success=`grep "Finished make all" hmake.log`

############### if we're good, EXECUTE HMAKE INSTALL ##################################
if [ "x$hmake_success" != "x" ] || [ "x$hmake_errors" == "x" ]; then
    if  [ "x$hmake_success" != "x" ]; then
	echo hmake success confirmed: $hmake_success
    fi

    if test "x$arg1" != "xnowarn"; then
	echo Warnings:
	grep "warning" hmake.log
	echo
    fi

    echo hmake install ...
    hmake install >& hmake_install.log
else
    stopdate=`\date`
    echo "Stop:  $stopdate" >> buildtime.log
    time_ellapsed "$startdate" "$stopdate" >> buildtime.log
    echo heasim hmake failed. >> buildtime.log    
    echo
    grep "error" hmake.log
    echo
    more buildtime.log
    exit 1
fi

# check for error messages in hmake_install.log
hmake_install_errors=`grep "]: \*\*\*" hmake_install.log`
hmake_install_success=`grep "Finished make install" hmake_install.log`

################ if we're good, append time info and exit ##############################

if [ "x$hmake_install_errors" == "x" ] || [ "x$hmake_install_success" != "x" ]; then 

    if test "x$hmake_install_success" != "x"; then
	echo hmake_install success confirmed: $hmake_install_success
    fi

    stopdate=`\date`
    echo "Stop:  $stopdate" >> buildtime.log
    time_ellapsed "$startdate" "$stopdate" >> buildtime.log
    echo heasim build successful. >> buildtime.log

else
    stopdate=`\date`
    echo "Stop:  $stopdate" >> buildtime.log
    time_ellapsed "$startdate" "$stopdate" >> buildtime.log
    echo heasim hmake install failed. >> buildtime.log
    more buildtime.log
    exit 1
fi

more buildtime.log

if [ "x$arg1" == "xcleanup" ]; then
    rm buildtime.log
    rm config.log
    rm hmake.log
    rm hmake_install.log
fi

cd $thisdir
exit 0





# $Log: build_simulator.sh,v $
# Revision 1.9  2015/04/14 20:59:21  driethmi
# Set default behavior to have --enable-mac_32bit=no.  Also set default
# behavior to show all compilation warnings.
#
# Revision 1.8  2014/05/29 18:35:04  driethmi
# Made sure optimizations were turned off for sim build.
#
# Revision 1.7  2014/04/23 19:13:10  driethmi
# Added line in build_simulator to source heasim-init.sh from the BUILD_DIR.
#
# Revision 1.6  2014/02/20 18:32:56  driethmi
# Corrected temporary change in build_simulator that was mistakenly committed.
#
# Revision 1.5  2014/02/20 18:30:48  driethmi
# Moved the doWork section in heasim.c main to its own function in doWork.c.
# Also moved C_ReturnChannel() to doWork.c and removed the ReturnChannel.c
# file from the repository.
#
# Revision 1.4  2014/01/14 15:40:41  driethmi
# Turns out querying the build information is unnecessary during the
# configure stage.
#
# Revision 1.3  2014/01/14 15:28:15  driethmi
# Replaced specific "driethmi" instances for directory paths with general-
# use paths.
#
# Revision 1.2  2013/12/18 20:55:08  driethmi
# Fixed minor bug - the string "***" is no longer taken to indicate an error,
# as it may indicate a pointer to a dynamically allocated 2D array.
#
# Revision 1.1  2013/11/27 15:34:36  driethmi
# Added new file build_simulator.sh to repository.  Essentially executes
# hmake distclean, configure, hmake, and hmake install, with some additional
# bells and whistles.
#





