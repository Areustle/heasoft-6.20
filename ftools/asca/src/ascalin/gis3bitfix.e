#!/bin/sh
# GIS3BITFIX.E, BY ERIC GOTTHELF (NASA/GSFC), 1994.
# Automatic script to fix science files effected by the GIS3 3 bit LSB error.
# 
#   Modification history:
#
#   Sept 1994, EVG. Modified to follow XPI change, replaced pquery -> pget.
#   Oct  1994, EVG. Modified to follow another XPI change, operation of mode=h.
#                   Added HISTORY card.
#    Jan 1998, EVG. Correctly sets the ra/dec values for PHA=256 mode.
#
a=gis3bitfix.e
if test $# -eq 2
then
if test $2 = rename
then
for file in `cat $1 | colrm 32`
do
echo 'renaming file : ' $file.fix
mv -i $file.fix $file
done
exit
fi
fi 
if test $# -lt 4
then
echo ' '
echo '   All GIS3 PH data acquired in PHA mode between Feb 10 and Apr 8 1994 suffer'
echo '   from an on-board software related problem. The 3 LSB PHA bits are stuck'
echo '   in the pattern 101; some time resolution may be lost. This script makes'
echo '   a set of reproccessed, fixed GIS3 PHA science files. However the digital'
echo '   resolution of the resultant spectra is necessarily decreased to 7 bits.'
echo '     '
echo '   The arguments are: 1) a file containing a list of (previously processed) '
echo '   effected GIS3 event files, 2) those neccessary to run ASCALIN.'
echo '   The resultant files have the extention .fix appended to the file name.'
echo ' '
echo '   Questions? e-mail ascahelp@legacy.gsfc.nasa.gov'
echo ' '
echo ' Usage: '$a' evtlist calfile timfile attfile     (Process evtlist files)'
echo '                    and'
echo ' Usage: '$a' evtlist rename   (Rename .fix files back to original names)'
echo ' '
echo ' where,'
echo '   evtlist = name of file containing a list of event files to be corrected'
echo '   calfile = name of teldef file       (the gis3_phnew* file)'
echo '   timfile = name of gain history file (the ft*.ghf     file)'
echo '   attfile = name of attitude file     (the fa*         file)'
echo '   rename  = literal string, to rename fixed files back to orginal names'
echo ' '
if test $# -ne 0
  then 
  echo ' Too few command line arguments, please try again.'
fi
exit
fi
calfile=$2
timfile=$3
attfile=$4
pointing=user
pset ascalin pointing=$pointing
pset ascalin calfile=$calfile
pset ascalin tempofile=$timfile
pset ascalin attitude=$attfile
for file in `cat $1`
do
echo 'processing file  : ' $file
if test -f $file
then
fkeypar $file+0 ORBITBEG
start_mjd=`pget fkeypar value | sed  "s/'/ /g"`
echo 'START ORBIT:  '$start_mjd
fkeypar $file+0 ORBITEND
stop_mjd=`pget fkeypar value  | sed  "s/'/ /g"`
echo 'STOP  ORBIT:  '$stop_mjd
if test $start_mjd -gt 94021000
then
if test $stop_mjd -lt 94040900
then
fkeypar $file+0 INSTRUME
instr=`pget fkeypar value  | sed  "s/'/ /g"`
echo 'INSTRUMENT :  ' $instr
if test $instr = GIS3
then
fkeypar $file+0 PHA_BINS
phabins=`pget fkeypar value | sed  "s/'/ /g"`
echo 'PHA_BINS   :   '$phabins
if test $phabins -eq 1024
then
fkeypar $file+0 TIMEBINS
timebins=`pget fkeypar value | sed  "s/'/ /g"`
echo 'TIMEBINS   :   '$timebins
fkeypar $file+0 BIT_RATE
bitrate=`pget fkeypar value | sed  "s/'/ /g"`
echo 'BIT_RATE   :  '$bitrate
fixfile=$file.fix
rm -f $fixfile
echo 'FIXED FILE : '$fixfile
fcalc $file+1 $fixfile PHA 'PHA/8' copycol=yes histkw=yes copyall=yes
rm -f gis3bitfix.kw
echo 'G3BITFIX= FIXED / The file created by the script gis3bitfix.e /' > gis3bitfix.kw
if test $timebins -eq 512
then
if test $bitrate -eq HIGH
then
timedel='1.220703E-4'
elif test $bitrate -eq MEDIUM
then
timedel='0.976525E-4'
elif test $bitrate -eq LOW
then
timedel='3.90625E-3'
fi
fparkey $timedel $fixfile+0 TIMEDEL
fparkey $timedel $fixfile+1 TIMEDEL
echo 'HISTORY    The timing has been reset to '$timedel' \'>> gis3bitfix.kw 
elif test $timebins -eq 1024
then
if test $bitrate -eq HIGH
then
timedel='6.103515E-5'
elif test $bitrate -eq MEDIUM
then
timedel='4.8828125E-4'
elif test $bitrate -eq LOW
then
timedel='1.953125E-3'
fi
fparkey $timedel $fixfile+0 TIMEDEL
fparkey $timedel $fixfile+1 TIMEDEL
echo 'HISTORY    The timing has been reset to '$timedel' \'>> gis3bitfix.kw 
fi
fparkey 128 $fixfile+0 PHA_BINS
fparkey 128 $fixfile+1 PHA_BINS
fparkey 127 $fixfile+1 TLMAX4
fparkey 0 $fixfile+1 TLMAX5
echo 'HISTORY   The PHA/PI values in this file has been reset to 7 bits' >> gis3bitfix.kw
fkeypar $fixfile+1 TCRVL2
ranom=`pget fkeypar value | sed  "s/'/ /g"`
fkeypar $fixfile+1 TCRVL3
decnom=`pget fkeypar value | sed  "s/'/ /g"`
pset ascalin ranom=$ranom
pset ascalin decnom=$decnom
pset ascalin datafile=$fixfile
pset ascalin mode=h
ascalin
fmodhead $fixfile+0 gis3bitfix.kw
fmodhead $fixfile+1 gis3bitfix.kw
elif test $phabins -eq 256
then
fkeypar $file+0 TIMEBINS
timebins=`pget fkeypar value | sed  "s/'/ /g"`
echo 'TIMEBINS   :   '$timebins
fkeypar $file+0 BIT_RATE
bitrate=`pget fkeypar value | sed  "s/'/ /g"`
echo 'BIT_RATE   :  '$bitrate
fixfile=$file.fix
rm -f $fixfile
echo 'FIXED FILE : '$fixfile
fcalc $file+1 $fixfile PHA 'PHA/2' copycol=yes histkw=yes copyall=yes
if test $timebins -eq 512
then
if test $bitrate -eq HIGH
then
timedel='1.220703E-4'
elif test $bitrate -eq MEDIUM
then
timedel='0.976525E-4'
elif test $bitrate -eq LOW
then
timedel='3.90625E-3'
fi
fparkey $timedel $fixfile+0 TIMEDEL
fparkey $timedel $fixfile+1 TIMEDEL
echo 'HISTORY    and the timing has been reset to '$timedel' \'>> gis3bitfix.kw 
elif test $timebins -eq 1024
then
if test $bitrate -eq HIGH
then
timedel='6.103515E-5'
elif test $bitrate -eq MEDIUM
then
timedel='4.8828125E-4'
elif test $bitrate -eq LOW
then
timedel='1.953125E-3'
fi
fparkey $timedel $fixfile+0 TIMEDEL
fparkey $timedel $fixfile+1 TIMEDEL
echo 'HISTORY    and the timing has been reset to '$timedel' \'>> gis3bitfix.kw 
fi
fparkey 128 $fixfile+0 PHA_BINS
fparkey 128 $fixfile+1 PHA_BINS
fparkey 127 $fixfile+1 TLMAX4
fparkey 0 $fixfile+1 TLMAX5
echo 'HISTORY   The PHA/PI values in this file has been reset to 7 bits' >> gis3bitfix.kw
fkeypar $fixfile+1 TCRVL2
ranom=`pget fkeypar value | sed  "s/'/ /g"`
fkeypar $fixfile+1 TCRVL3
decnom=`pget fkeypar value | sed  "s/'/ /g"`
pset ascalin ranom=$ranom
pset ascalin decnom=$decnom
pset ascalin datafile=$fixfile
pset ascalin mode=h
ascalin
fmodhead $fixfile+0 gis3bitfix.kw
fmodhead $fixfile+1 gis3bitfix.kw
else
echo ' Data mode not effected (!?!): skipping file...'
fi
else
echo ' Not effected instrument (!?!): skipping file...'
fi
else
echo ' Observation after effected times (!?!): skipping file...'
fi
else
echo ' Observation precceds effected times (!?!): skipping file...'
fi
else
echo ' File not found. Skipping...'
fi
done
pset ascalin mode=ql
exit
