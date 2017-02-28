#!/bin/sh
#
# ASCALIN.E, BY ERIC GOTTHELF (NASA/GSFC), 1994.
# Automatic script for running ASCALIN on multiple input file.
# 
a=ascalin.e
if test $# -lt 5
then
echo ' '
echo '     Automatic script for running ASCALIN on multiple input files.'
echo '    Please read help page (fhelp ascalin) for further information.' 
echo ' '
#echo ' usage: '$a' evtlist calfile timfile attfile [E1 E2 E3]'
#echo '    or:                                           pointing [RA DEC]'
#echo ' '
echo '        Aspect each science file to its mean pointing (for that file):'
echo ' usage: '$a' evtlist calfile timfile attfile ATT'
echo ' '
echo '    or: Aspect science files to user selected RA/DEC' 
echo ' usage: '$a' evtlist calfile timfile attfile USER RA DEC'
echo ' '
echo '    or: Fixed aspect using Euler angles E1/E2/E3'
echo ' usage: '$a' evtlist calfile timfile EULER E1 E2 E3'
echo ' '
echo ' where,'
echo '   evtlist  = file containing a list of event files to be processed'
echo '   calfile = name of teldef file              (the gis3_phnew* file)'
echo '   timfile = name of gain history file        (the ft*.ghf     file)'
echo '   attfile = name of attitude file            (the fa* file; or "EULER")'
echo '   E1,E2,E3 = Euler angles to use if attfile = EULER'
echo '   pointing = method to determine aspect point (ATT->file mean, USER)'
echo '   RA,DEC   = aspect point if pointing = USER'
echo ' '
echo ' ex1: Aspect each science file to its mean pointing (for that file):'
echo '   '$a' evtlist calfile timfile attfile ATT'
echo ' '
echo ' ex2: Aspect science files to a common (USER supplied) RA/DEC:' 
echo '   '$a' evtlist calfile timfile attfile USER 90.9 65.2'
echo ' '
echo ' ex3: Do a fixed aspect assuming a set of Euler angles:'
echo '   '$a' evtlist calfile timfile EULER 90.9 24.8 1.0'
#echo ' '
#echo ' ex4: Use euler angle but with explicit calibation files:'
#echo '   '$a' evtlist teldef.fits gainhist.fits EULER 90.9 24.8 1.0'
#echo ' '
#echo ' ex5: Use attitude file mean pointing with explicit calib. files:'
#echo '   '$a' evtlist teldef.fits gainhist.fits attitude.fits ATT'
#echo ' '
#echo ' ex6: Use user RA/DEC to aspect science files w/ explicit files:'
#echo '   '$a' evtlist teldef.fits gainhist.fits attitude.fits USER 90.9 65.2'
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
pset ascalin calfile=$calfile
pset ascalin tempofile=$timfile
pset ascalin attitude=$attfile
#pset ascalin defGISfile=~/pub/pv.ghf
#pset ascalin defATTpath=/imp/asca1/data/old_historical_trend/attitude
if test $attfile = fixedasp -o $attfile = euler -o $attfile = none -o \
$attfile = FIXEDASP -o $attfile = EULER -o $attfile = NONE
then
 pset ascalin eulerphi=$5
 pset ascalin eulertheta=$6
 pset ascalin eulerpsi=$7
else
 pointing=$5
 pset ascalin pointing=$pointing
 if test $pointing = user -o $pointing = USER
 then
  pset ascalin ranom=$6
  pset ascalin decnom=$7
 fi
fi
pset ascalin gainnorm=1.0
pset ascalin gainoff=0.0
pset ascalin mode=h
for file in `cat $1`
do
echo 'processing file  : ' $file
ascalin $file
done
pset ascalin mode=ql
exit
