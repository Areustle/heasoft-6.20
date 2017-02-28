#! /bin/sh
# \file ahsysinfo
# \brief Query the system for the bit types of the processor, the firmware,
# the kernel, and (optionally) an specified executable.
# \author David Riethmiller

#---------------------------------------------------------------------------
# This script queries the system for various information, including 
# the kernel operating system, the processor name, the firmware version (if
# possible), and the bit types of each.  We use common terminal commands
# to do this...the advantage of this script is that the commands are 
# bundled neatly together and print out succint information.  Note that
# we change NOTHING about the system; read and report only.
#
#---------------------------------------------------------------------------

arg1=$1
arg2=$2

case "x$arg1" in
    "x-v") flag="version";;
    "x-V") flag="version";;
    "x-h") flag="help";;
    "x--help") flag="help";;
    "x-verbose") flag="verbose";;
    *) flag="noflags";;
esac
	
if test "$flag" = "noflags"; then
    exName=$1;
else 
    exName=$2;
fi

orig_exName=`echo $exName`

# Allow user to query executables in PATH regardless of current location.
# Note that entering "ahsysinfo aht" will grab info from aht in the PATH, while
# entering "ahsysinfo ./aht" will grab info from an executable in the current
# directory.
if test "x$exName" != "x"; then
    exName=`which $exName`
fi


totRAM="??"

if test "$flag" = "version"; then
    echo
    echo ahsysinfo version:
    grep -m2 "Revision" $HEADAS/bin/ahsysinfo | tail -n1
    echo
    exit 1
fi


echo
echo WELCOME TO AHSYSINFO! Querying system information...
echo


if test "$flag" = "help"; then
    echo \
'
Usage: ahsysinfo [options] [executable]

This tool queries the system and reports the OS, processor, firmware
(if applicable), and kernel bit types.  The default behavior is to print
only the system build information.  If the optional argument
[executable] is provided, this tool also reports the build information
for that binary executable.

In the case where an executable is given, this script should report
the executable name, the bit type of the executable, and if there 
exists a GCC version number in the unstripped strings, we report
that as well.

This utility replaces ahsysinfo, replicating the library information
and the src and header versions, when given the -verbose option.

The options are:

   [executable name]  Display the build info for the executable
   -h --help          Display this information
   -v -V              Display the version information
   -verbose           Display additional information as per ahsysinfo.

Examples:
1. Report the system information:
     %> ahsysinfo

2. Report the system information as well as the build type for the
   ahtime binary (located in current directory):
     %> ahsysinfo ahtime

3. Report the current version of getBitType:
     %> ahsysinfo -v

4. Report all available information for ahtime:
     %> getBitType -verbose ahtime

' >&2
exit 1
fi

SysType=`uname -s`

###########################################################
############ IF WE'RE DEALING WITH MAC ####################
###########################################################
if [ "$SysType" = "Darwin" ]; then

    # Get information from the system_profiler *********************
    
    # Read the hardware info into a temporary file, then read that
    # file line by line, extracting only relevant info.
    procname=`system_profiler SPHardwareDataType | grep "Processor Name"`
    procname=`echo $procname | cut -d : -f2`

    ModID=`system_profiler SPHardwareDataType | grep "Model Identifier"`
    ModID=`echo $ModID | cut -d : -f2`

    # Get these also, but we probably won't need them. *****************
    bootROM=`system_profiler SPHardwareDataType | grep "Boot ROM Version"`
    bootROM=`echo $bootROM | cut -d : -f2`

    SMC=`system_profiler SPHardwareDataType | grep "SMC Version (system)"`
    SMC=`echo $procname | cut -d : -f2`
    # ******************************************************************

    # Read the software info in analogous way to hardware info.
    sysversion=`system_profiler SPSoftwareDataType | grep "System Version"`
    sysversion=`echo $sysversion | cut -d : -f2`
    
    # Compare system information against known bit types.  May need
    #to add to this list as newer CPUs become available.
    procname=`echo $procname` # not sure why this is necessary...
    case $procname in
	"Intel Core Solo") procbit="matches known 32 bit";;
	"Intel Core Duo") procbit="matches known 32 bit";;
	"Intel Core 2 Duo") procbit="matches known 64 bit";;
	"Intel Quad-Core Xeon") procbit="matches known 64 bit";;
	"Dual-Core Intel Xeon") procbit="matches known 64 bit";;
	"Quad-Core Intel Xeon") procbit="matches known 64 bit";;
	"Intel Core i3") procbit="matches known 64 bit";;
	"Intel Core i5") procbit="matches known 64 bit";;
	"Intel Core i7") procbit="matches known 64 bit";;
	*) procbit="unrecognized";;
    esac
    
    # Get Extensible Firmware Interface (EFI) information from the
    # I/O Kit Registry.
    EFI=`ioreg -l -p IODeviceTree | grep firmware-abi`
    EFI=`echo $EFI | cut -d \< -f2` # isolate the relevant part of the string
    EFI=`echo $EFI | cut -d \" -f2` # ...
    case $EFI in
        "EFI64") EFItype="EFI 64 bit";;
        "EFI32") EFItype="EFI 32 bit";;
        *) EFItype="unrecognized";;
    esac

    # The 'uname' command gives info for the KERNEL, unrelated to hardware.
    kern=`uname -m`
    case $kern in
	"i386") kernbit="32 bit";;
	"i486") kernbit="32 bit";;
	"i586") kernbit="32 bit";;
	"i686") kernbit="32 bit";;
	"x86_64") kernbit="64 bit";;
	*) kernbit="unrecognized";;
    esac

    totRAM=`system_profiler SPHardwareDataType | grep "Memory:" | awk -F ' ' '{print $2}'`

##########################################################
############# IF WE'RE DEALING WITH LINUX ################
##########################################################
elif [ "$SysType" = "Linux" ]; then

    # Still need to figure out how to query the system to
    # report the type of machine that we build on, e.g.
    # "Acer Aspire1 Netbook" or "Dell Precision T1700"
    ModID="<Linux Box>"

    # Read the operating system from lsb_release
    sysversion=`lsb_release -d | cut -d : -f2`

    # Read the processor name out of /proc/cpuinfo.  There 
    # may be more than one processor entry; we count the number
    # of ":" instances in the string and append this to $procname.
    #
    # Would be good later to add a check that all processors in the
    # string are the same type...
    procname=`grep "model name" /proc/cpuinfo | sed 's/model name//g'`
    numproc=`echo $procname | tr -dc ':' | wc -c`
    procname=`echo $procname | cut -d : -f2`
    procname="$procname ($numproc)"

    # If the flag "lm" is present in the list of flags in /proc/cpuinfo,
    # then the processor is a 64bit architecture.  If not, it is 32bit.
    cpuflags=`grep "flags" /proc/cpuinfo`
    cpuflags=`echo $cpuflags | cut -d : -f2`
    findlm=`echo $cpuflags | grep -c " lm"`
    case $findlm in
	0) procbit="32 bit";;
	1) procbit="64 bit";;
	*) procbit="unknown";;  # this shouldn't be possible
    esac

    # Don't know if the Linux firmware needs to be considered here
    # or not...don't know command to query.
    EFItype="N/A"
    
    # Query the system via "uname" for kernel information.
    kern=`uname -m`
    case $kern in
        "i386") kernbit="32 bit";;
        "i486") kernbit="32 bit";;
        "i586") kernbit="32 bit";;
        "i686") kernbit="32 bit";;
        "x86_64") kernbit="64 bit";;
        *) kernbit="unrecognized";;
    esac

    totRAM=`grep MemTotal /proc/meminfo | awk -F ' ' '{print $2}'`
    totRAM=`echo "$totRAM / 1000000." | bc`

########################################################### 
############# IF WE'RE DEALING WITH CYGWIN ################
########################################################### 
elif [ `echo $SysType|awk '{print match($0,"CYGWIN")}'` -gt 0 ]; then

    ModID="<Windows Box>"

    # Read the processor name out of /proc/cpuinfo.  There
    # may be more than one processor entry; we count the number
    # of ":" instances in the string and append this to $procname.
    #
    # Would be good later to add a check that all processors in the
    # string are the same type...

    procname=`grep "model name" /proc/cpuinfo | sed 's/model name//g'`
    numproc=`echo $procname | tr -dc ':' | wc -c`
    procname=`echo $procname | cut -d : -f2`
    procname="$procname ($numproc)"

    # If the flag "lm" is present in the list of flags in /proc/cpuinfo,
    # then the processor is a 64bit architecture.  If not, it is 32bit.
    cpuflags=`grep "flags" /proc/cpuinfo`
    cpuflags=`echo $cpuflags | cut -d : -f2`
    findlm=`echo $cpuflags | grep -c " lm"`
    case $findlm in
        0) procbit="32 bit";;
        1) procbit="64 bit";;
        *) procbit="unknown";;  # this shouldn't be possible
    esac

    # Don't know if the Linux firmware needs to be considered here
    # or not...don't know command to query.
    EFItype="N/A"

    # Get the operating system running Cygwin
    osname=`systeminfo | grep "^OS Name" | awk -F ':' '{print $2}'`
    osvers=`systeminfo | grep "^OS Version" | awk -F ':' '{print $2}'`
    sysversion="$osname $osvers"


     # Query the system via "uname" for kernel information.
    kern=`uname -m`
    case $kern in
        "i386") kernbit="32 bit";;
        "i486") kernbit="32 bit";;
        "i586") kernbit="32 bit";;
        "i686") kernbit="32 bit";;
        "x86_64") kernbit="64 bit";;
        *) kernbit="unrecognized";;
    esac


else

    echo "Kernel name is $SysType; only Darwin, Linux, and Cygwin are supported."

fi

###################################################################
##################### Report System Results #######################
###################################################################
echo
echo "*************** System Build Information *********************"
echo
echo System Type: $SysType
echo Model ID: $ModID
echo System Version: $sysversion
#echo Boot ROM Version: $bootROM
#echo SMC Version: $SMC
echo Processor Name: $procname 
echo Processor BitType: $procbit 
echo EFI: $EFItype # this may only be relevant for Mac
echo Kernel: $kern \($kernbit\)
echo System RAM: $totRAM GB
echo

# cygwin may have other relevant information
if [ `echo $SysType|awk '{print match($0,"CYGWIN")}'` -gt 0 ]; then
    echo "Additional Cygwin system info:"
    systeminfo
    echo
fi


# If there is a binary argument provided, check the build type of that 
# executable.

####################################################################
####################### Binary Executable Query ####################
####################################################################

echo "************* Executable Build Information *******************"
echo Name:
if test "x$exName" = "x"; then  # If user doesn't specify ex
    echo "<No executable specified>"
else
    echo $exName  # Otherwise print ex name.

    # If ex is a symbolic link, then track down the target.
    if [ -L $exName ]; then
	echo
	echo "*** Given executable is symbolic link, redirecting to:"
	linkdir=`dirname $exName`         # absolute directory containing link
	actualEx=`readlink $exName`       # where does the link redirect
	fullpath=$linkdir/$actualEx       # full (unnormalized) path to target
	actual_exdir=`dirname $fullpath`  # full (unnormalized) directory containing target

	# get normalized target directory name, i.e. remove "../../"
	normalDir=`cd "$actual_exdir"; pwd`
	
	# append executable name to target directory
	exName=$normalDir/$orig_exName

	echo $exName
    fi
	
fi
echo

echo Bit Type: 
if test "x$exName" = "x"; then
    echo "<No executable specified>"
else
    exInfo=`file $exName`
    exInfo=`echo $exInfo | cut -d : -f2`
    echo $exInfo # Note that shell scripts won't display bit type.
fi
echo

    # Check to see if we've called getBitType on itself...if so, then
    # the gcc grep calls will return themselves verbatim.
if test "x$exName" = "xahsysinfo" -a "x$exName" = "xgetBitType"; then
    echo You have called getBitType on its own executable.
    echo
else 
    if test "x$exName" != "x"; then
	    # This may not show up in the strings command, i.e. if there was
	    # no gcc compilation, or is the binary has been stripped.
	gccBuild=`strings -a $exName | grep "GCC:" | sort -u`
	if test "x$gccBuild" != "x"; then
	    echo "Compiled with gcc version (gcc or gfortran):"
	    echo $gccBuild
	    echo
	fi
    fi
fi


if test "$flag" = "verbose"; then
    echo Built with libraries:
    if [ "$SysType" = "Linux" ]; then
	libs=`ldd $exName`
	ldd $exName
    elif [ "$SysType" = "Darwin" ]; then
	libs=`otool -L $exName`
	otool -L $exName
    fi
    if test ${#libs} = 0; then
	echo "sorry, dynamic libs unknown..."
    fi
    echo
    
    echo "src & header versions:"
    echo
    strings $exName | grep '\$Id' | sort -u
    echo

fi

    echo "**************************************************************"
    echo
    echo

exit 0



#
# $Log: ahsysinfo.pl,v $
# Revision 1.1  2015/08/13 19:59:33  mwitthoe
# moving ahsysinfo from gen/aht into its own directory under gen/tasks
#
# Revision 1.14  2015/06/12 18:12:03  driethmi
# Added check for system RAM.
#
# Revision 1.13  2014/05/07 20:13:57  driethmi
# Added support for Cygwin kernel.
#
# Revision 1.12  2013/11/13 20:02:43  driethmi
# Minor changes.  Added explicit check to see if an executable is specified
# before calling "which exName" since otherwise this call makes no sense.
#
# Revision 1.11  2013/11/04 22:45:56  driethmi
# Added block which checks to see if given executable is a symbolic link.  If
# so, it follows the link back to the target executable, and reports info
# for the target, rather than for the link.
#
# Revision 1.10  2013/10/31 15:53:47  driethmi
# Allow user to query executable in $PATH, regardless of current location.
#
# Revision 1.9  2013/09/19 15:50:38  driethmi
# Updated to make sure that ahsysinfo -v grabs revision info from
# $HEADAS/bin/ahsysinfo, rather than assuming ahsysinfo lives in the
# current directory.
#
# Revision 1.8  2013/09/19 14:06:21  driethmi
# Updated so that ahsysinfo -v actually grabs the most recent cvs revision
# information.
#
# Revision 1.7  2013/09/18 18:00:43  driethmi
# Removed getBitType.sh, since it is now written into the new ahsysinfo.  Adjusted
# some of the comments in ahsysinfo.
#
# Revision 1.6  2013/09/18 17:35:25  driethmi
# Replaced the old ahsysinfo with a more functional utility.  Reports both
# system information and executable information.  Type ahsysinfo -h to see
# the available options.
#
#
# Revision 0.3 2013/08/30 15:12:00  driethmi
# Changed the Linux "System Version" query to use the command "lsb_release",
# since Scientific Linux populates its /etc/issue file with different
# information.  Also changed the default behavior to report that no
# executable name was given, and cleaned up this section of the code.
#
# Revision 0.2 2013/08/30 09:52:00  driethmi
# Add log information macro
#